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
import Control.Monad (forM_)
import System.FilePath ((</>))

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
            route $ setExtension "html"
            compile $ do
                let (prevFile, prevTitle) = maybe ("", "") (\(f, _, t) -> (toFilePath $ setExtension "html" $ fromFilePath f, t)) mprev
                    (nextFile, nextTitle) = maybe ("", "") (\(f, _, t) -> (toFilePath $ setExtension "html" $ fromFilePath f, t)) mnext
                
                pandocCompiler
                    >>= loadAndApplyTemplate "config/template.html" 
                            (chapterContext title prevFile prevTitle nextFile nextTitle)
                    >>= postProcessImages
    
    -- Generate chapters.html (TOC)
    create ["chapters.html"] $ do
        route idRoute
        compile $ do
            headContent <- unsafeCompiler $ readFile "source_md/chapters_head.md"
            footContent <- unsafeCompiler $ readFile "source_md/chapters_foot.md"
            
            -- Build TOC from all chapters using Pandoc to extract headings
            tocLines <- forM chapterFiles $ \(fname, idx, title) -> do
                let htmlName = toFilePath $ setExtension "html" $ fromFilePath fname
                    sp = if idx >= 10 then " " else "  "
                    chapterLine = show idx ++ "." ++ sp ++ "[" ++ title ++ "](" ++ htmlName ++ ")"
                
                -- Load chapter markdown, parse with Pandoc, extract subsections
                item <- load (fromFilePath $ "source_md/" ++ fname)
                pandoc <- readPandocWith defaultHakyllReaderOptions item
                let subsections = extractTOCFromPandoc htmlName (itemBody pandoc)
                
                return $ chapterLine : subsections
            
            let tocContent = unlines $ concat tocLines
                fullContent = headContent ++ "\n" ++ tocContent ++ "\n" ++ footContent
            
            makeItem fullContent
                >>= renderPandoc
                >>= loadAndApplyTemplate "config/template.html"
                        (constField "title" "Chapters - Learn You a Haskell for Great Good!" <>
                         defaultContext)
                >>= postProcessChaptersList
    
    -- Generate faq.html
    match "source_md/faq.md" $ do
        route $ setExtension "html"
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
    let pattern = "source_md/*.md"
    ids <- getMatches (fromGlob pattern)
    chapters <- mapM getChapterData $ filter (not . isFaqOrHelper . toFilePath) ids
    return $ sortOn (\(_, idx, _) -> idx) chapters
  where
    getChapterData :: Identifier -> IO (FilePath, Int, String)
    getChapterData ident = do
        let fname = toFilePath ident
        meta <- loadMetadata ident
        let order = fromMaybe (error $ "Missing chapter ID in metadata for: " ++ fname) $ lookupInt "chapter" meta
        
        -- Extract title from Pandoc AST
        content <- readFile fname
        pandoc <- runIO $ readMarkdown defaultHakyllReaderOptions (T.pack content)
        title <- case pandoc of
            Right (Pandoc _ blocks) -> return $ extractFirstHeading blocks
            Left err -> error $ "Failed to parse " ++ fname ++ ": " ++ show err
        
        return (fname, order, title)
    
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
        [(level, anchor, inlineToString inlines)]
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

-- Context for chapter pages  
chapterContext :: String -> FilePath -> String -> FilePath -> String -> Context String
chapterContext title prevFile prevTitle nextFile nextTitle =
    constField "title" title <>
    constField "footdiv" "true" <>
    constField "prev_filename" prevFile <>
    constField "prev_title" prevTitle <>
    constField "next_filename" nextFile <>
    constField "next_title" nextTitle <>
    defaultContext

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

-- Helper to look up integer from metadata
lookupInt :: String -> Metadata -> Maybe Int
lookupInt key meta = case lookupString key meta of
    Just s -> case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
    Nothing -> Nothing
