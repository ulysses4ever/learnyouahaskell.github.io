{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.List (sortOn, find, isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import System.Directory (listDirectory)
import Control.Monad (forM_, forM)
import System.FilePath ((</>), replaceExtension, takeBaseName, takeExtension)

-- Helper function to pair each element with its previous and next elements
zipPrevNext :: [a] -> [(Maybe a, a, Maybe a)]
zipPrevNext xs = zip3 (Nothing : map Just xs) xs (map Just (tail xs) ++ [Nothing])

main :: IO ()
main = hakyllWith config $ do
    -- Copy static assets managed by Hakyll
    let copyDocs pat = match pat $ do
            route $ gsubRoute "static/" (const "")
            compile copyFileCompiler

    mapM_ copyDocs
        [ "static/assets/**"
        , "static/sh/**"
        , "static/index.html"
        , "static/robots.txt"
        , "static/sitemap.xml"
        ]
    
    -- Template
    match "config/template.html" $ compile templateBodyCompiler
    
    -- Collect all chapters with their metadata
    chapterFiles <- buildChapterList
    
    -- Process chapter markdown files
    let chapterTriples = zipPrevNext chapterFiles
    forM_ chapterTriples $ \(mprev, (fname, idx, title), mnext) -> do
        match (fromGlob $ "source_md" </> fname) $ do
            route $ customRoute $ \ident -> 
                let path = toFilePath ident
                    basename = takeBaseName path
                in basename ++ ".html"
            compile $ do
                let ctx = case (mprev, mnext) of
                        (Nothing, Nothing) -> 
                            constField "title" title <> 
                            constField "footdiv" "true" <>
                            defaultContext
                        (Nothing, Just (nf, _, nt)) ->
                            constField "title" title <>
                            constField "footdiv" "true" <>
                            constField "next_filename" (replaceExtension nf ".html") <>
                            constField "next_title" nt <>
                            defaultContext
                        (Just (pf, _, pt), Nothing) ->
                            constField "title" title <>
                            constField "footdiv" "true" <>
                            constField "prev_filename" (replaceExtension pf ".html") <>
                            constField "prev_title" pt <>
                            defaultContext
                        (Just (pf, _, pt), Just (nf, _, nt)) ->
                            constField "title" title <>
                            constField "footdiv" "true" <>
                            constField "prev_filename" (replaceExtension pf ".html") <>
                            constField "prev_title" pt <>
                            constField "next_filename" (replaceExtension nf ".html") <>
                            constField "next_title" nt <>
                            defaultContext
                
                pandocCompiler
                    >>= loadAndApplyTemplate "config/template.html" ctx
                    >>= postProcessImages
    
    -- Generate chapters.html (TOC)
    create ["chapters.html"] $ do
        route idRoute
        compile $ do
            headContent <- unsafeCompiler $ readFile "source_md/chapters_head.md"
            footContent <- unsafeCompiler $ readFile "source_md/chapters_foot.md"
            
            -- Build TOC from all chapters using Pandoc to extract headings
            tocLines <- forM chapterFiles $ \(fname, idx, title) -> do
                let htmlName = replaceExtension fname ".html"
                    sp = if idx >= 10 then " " else "  "
                    chapterLine = show idx ++ "." ++ sp ++ "[" ++ title ++ "](" ++ htmlName ++ ")"
                
                -- Load chapter markdown, parse with Pandoc, extract subsections
                item <- load (fromFilePath $ "source_md" </> fname)
                pandoc <- readPandocWith defaultHakyllReaderOptions item
                let subsections = extractTOCFromPandoc htmlName (itemBody pandoc)
                
                return $ chapterLine : subsections
            
            let tocContent = unlines $ concat tocLines
                fullContent = headContent ++ "\n" ++ tocContent ++ "\n" ++ footContent
            
            -- Use pandocCompiler to convert markdown to HTML
            makeItem fullContent
                >>= renderPandoc
                >>= loadAndApplyTemplate "config/template.html"
                        (constField "title" "Chapters - Learn You a Haskell for Great Good!" <>
                         defaultContext)
                >>= postProcessChaptersList
    
    -- Generate faq.html
    match "source_md/faq.md" $ do
        route $ customRoute $ const "faq.html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "config/template.html"
                        (constField "title" "FAQ - Learn You a Haskell for Great Good!" <>
                         constField "faq" "true" <>
                         defaultContext)
                >>= postProcessImages

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "_site"
    , storeDirectory = "_hakyll_cache"
    , tmpDirectory = "_hakyll_tmp"
    , providerDirectory = "."
    }

-- Build list of chapters sorted by chapter number from YAML metadata
buildChapterList :: Rules [(FilePath, Int, String)]
buildChapterList = preprocess $ do
    files <- listDirectory "source_md"
    let mdFiles = filter (\f -> takeExtension f == ".md") files
        chapterFiles = filter (not . isFaqOrHelper) mdFiles
    chapters <- mapM getChapterData chapterFiles
    return $ sortOn (\(_, idx, _) -> idx) chapters
  where
    getChapterData :: FilePath -> IO (FilePath, Int, String)
    getChapterData fname = do
        let fullPath = "source_md" </> fname
        content <- readFile fullPath
        
        -- Extract chapter number from YAML frontmatter
        let order = extractChapterNumber fullPath content
        
        -- Extract title from Pandoc AST
        pandoc <- runIO $ readMarkdown defaultHakyllReaderOptions (T.pack content)
        title <- case pandoc of
            Right (Pandoc _ blocks) -> return $ extractFirstHeading blocks
            Left err -> error $ "Failed to parse " ++ fullPath ++ ": " ++ show err
        
        -- Return just the filename, not the full path
        return (fname, order, title)
    
    extractChapterNumber :: FilePath -> String -> Int
    extractChapterNumber fname content =
        case lines content of
            [] -> error $ "Empty file: " ++ fname
            ("---":rest) -> parseYamlChapter fname rest
            _ -> error $ "No YAML frontmatter found in: " ++ fname
      where
        parseYamlChapter fname [] = error $ "YAML frontmatter not closed in: " ++ fname
        parseYamlChapter fname ("---":_) = error $ "No chapter field in YAML in: " ++ fname
        parseYamlChapter fname (line:rest)
            | "chapter:" `isPrefixOf` line =
                case reads (dropWhile (== ' ') $ drop 8 line) of
                    [(n, "")] -> n
                    _ -> error $ "Invalid chapter number in " ++ fname ++ ": " ++ line
            | otherwise = parseYamlChapter fname rest
    
    isFaqOrHelper :: FilePath -> Bool
    isFaqOrHelper fname = any (`isInfixOf` fname) ["faq.md", "chapters_head.md", "chapters_foot.md"]
      where
        isInfixOf needle [] = False
        isInfixOf needle haystack@(x:xs)
          | needle `isPrefixOf` haystack = True
          | otherwise = isInfixOf needle xs

-- Extract first heading from Pandoc blocks
extractFirstHeading :: [Block] -> String
extractFirstHeading blocks = 
    case find isHeader blocks of
        Just (Header _ _ inlines) -> inlineToString inlines
        _ -> ""
  where
    isHeader (Header 1 _ _) = True
    isHeader _ = False
    
    inlineToString :: [Inline] -> String
    inlineToString = query getString
    
    getString :: Inline -> String
    getString (Str s) = T.unpack s
    getString Space = " "
    getString (Code _ s) = T.unpack s
    getString _ = ""

-- Extract TOC from Pandoc document using Pandoc's AST
extractTOCFromPandoc :: String -> Pandoc -> [String]
extractTOCFromPandoc htmlName (Pandoc _ blocks) =
    let headers = query getHeader blocks
    in mapMaybe (makeLink htmlName) headers
  where
    getHeader :: Block -> [(Int, String, String)]
    getHeader (Header level (anchor, _, _) inlines) =
        [(level, T.unpack anchor, inlineToString inlines)]
    getHeader _ = []
    
    inlineToString :: [Inline] -> String
    inlineToString = query getString
    
    getString :: Inline -> String
    getString (Str s) = T.unpack s
    getString Space = " "
    getString (Code _ s) = T.unpack s
    getString _ = ""
    
    makeLink :: String -> (Int, String, String) -> Maybe String
    makeLink htmlName (level, anchor, title)
        | level == 2 = Just $ "    * [" ++ title ++ "](" ++ htmlName ++ "#" ++ anchor ++ ")"
        | otherwise = Nothing

-- Post-process images
postProcessImages :: Item String -> Compiler (Item String)
postProcessImages item = return $ fmap processHtml item
  where
    processHtml html = replace " />" ">" html
    replace old new str =
        case breakOn old str of
            (before, "") -> before
            (before, rest) -> before ++ new ++ replace old new (drop (length old) rest)
    breakOn needle haystack = go needle haystack []
      where
        go ndl [] acc = (reverse acc, [])
        go ndl hstk acc
            | ndl `isPrefixOf` hstk = (reverse acc, hstk)
            | otherwise = case hstk of
                (x:xs) -> go ndl xs (x:acc)
                [] -> (reverse acc, [])

-- Post-process chapters list
postProcessChaptersList :: Item String -> Compiler (Item String)
postProcessChaptersList item = return $ fmap addChaptersClass item
  where
    addChaptersClass = replaceFirst "<ol" "<ol class=\"chapters\""
    replaceFirst old new str = 
        case breakOn old str of
            (before, "") -> before
            (before, rest) -> before ++ new ++ drop (length old) rest
    breakOn needle haystack = go needle haystack []
      where
        go ndl [] acc = (reverse acc, [])
        go ndl hstk acc
            | ndl `isPrefixOf` hstk = (reverse acc, hstk)
            | otherwise = case hstk of
                (x:xs) -> go ndl xs (x:acc)
                [] -> (reverse acc, [])
