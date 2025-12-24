{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.List (sortOn, find, isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query)
import Text.Pandoc.Class (runIO, runIOorExplode)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Options (def, readerExtensions, writerExtensions, Extension(Ext_implicit_figures), ReaderOptions)
import Text.Pandoc.Extensions (disableExtension)
import System.Directory (listDirectory)
import Control.Monad (forM_, forM)
import System.FilePath ((</>), replaceExtension, takeBaseName, takeExtension)

-- Helper function to pair each element with its previous and next elements
zipPrevNext :: [a] -> [(Maybe a, a, Maybe a)]
zipPrevNext xs = zip3 (Nothing : map Just xs) xs (map Just (tail xs) ++ [Nothing])

-- Custom reader options that disable implicit_figures extension
customReaderOptions :: ReaderOptions
customReaderOptions = defaultHakyllReaderOptions
  { readerExtensions = disableExtension Ext_implicit_figures 
                      (readerExtensions defaultHakyllReaderOptions)
  }

-- Custom pandoc compiler that uses our custom reader options
customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWith customReaderOptions defaultHakyllWriterOptions

-- Helper function to build chapter context with optional prev/next navigation
chapterCtx :: Maybe (FilePath, Int, String, [String]) -> Maybe (FilePath, Int, String, [String]) -> String -> Context String
chapterCtx mprev mnext title =
    constField "title" title <>
    constField "footdiv" "true" <>
    maybe mempty (\(pf, _, pt, _) -> 
        constField "prev_filename" (replaceExtension pf ".html") <>
        constField "prev_title" pt) mprev <>
    maybe mempty (\(nf, _, nt, _) ->
        constField "next_filename" (replaceExtension nf ".html") <>
        constField "next_title" nt) mnext <>
    defaultContext

main :: IO ()
main = hakyll $ do
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
    forM_ chapterTriples $ \(mprev, (fname, idx, title, _), mnext) -> do
        match (fromGlob $ "source_md" </> fname) $ do
            route $ gsubRoute "source_md/" (const "") `composeRoutes` setExtension "html"
            compile $ do
                let ctx = chapterCtx mprev mnext title
                
                customPandocCompiler
                    >>= loadAndApplyTemplate "config/template.html" ctx
                    >>= postProcessImages
    
    -- Generate chapters.html (TOC)
    create ["chapters.html"] $ do
        route idRoute
        compile $ do
            headContent <- unsafeCompiler $ readFile "source_md/chapters_head.md"
            footContent <- unsafeCompiler $ readFile "source_md/chapters_foot.md"
            
            -- Build TOC from all chapters using pre-computed subsections
            let buildChapterTOC (fname, idx, title, subsections) =
                    let htmlName = replaceExtension fname ".html"
                        sp = if idx >= 10 then " " else "  "
                        chapterLine = show idx ++ "." ++ sp ++ "[" ++ title ++ "](" ++ htmlName ++ ")"
                    in chapterLine : subsections
                tocLines = concatMap buildChapterTOC chapterFiles
                tocContent = unlines tocLines
                fullContent = headContent ++ "\n" ++ tocContent ++ "\n" ++ footContent
            
            -- Convert markdown to HTML using Pandoc directly
            htmlContent <- unsafeCompiler $ do
                result <- runIOorExplode $ do
                    pandocDoc <- readMarkdown customReaderOptions (T.pack fullContent)
                    writeHtml5String def pandocDoc
                return (T.unpack result)
            
            makeItem htmlContent
                >>= loadAndApplyTemplate "config/template.html"
                        (constField "title" "Chapters - Learn You a Haskell for Great Good!" <>
                         defaultContext)
                >>= postProcessChaptersList
    
    -- Generate faq.html
    match "source_md/faq.md" $ do
        route $ gsubRoute "source_md/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            customPandocCompiler
                >>= loadAndApplyTemplate "config/template.html"
                        (constField "title" "FAQ - Learn You a Haskell for Great Good!" <>
                         constField "faq" "true" <>
                         defaultContext)
                >>= postProcessImages



-- Build list of chapters sorted by chapter number from YAML metadata
buildChapterList :: Rules [(FilePath, Int, String, [String])]
buildChapterList = preprocess $ do
    files <- listDirectory "source_md"
    let mdFiles = filter (\f -> takeExtension f == ".md") files
        chapterFiles = filter (not . isFaqOrHelper) mdFiles
    chapters <- mapM getChapterData chapterFiles
    return $ sortOn (\(_, idx, _, _) -> idx) chapters
  where
    getChapterData :: FilePath -> IO (FilePath, Int, String, [String])
    getChapterData fname = do
        let fullPath = "source_md" </> fname
        content <- readFile fullPath
        
        -- Extract chapter number from YAML frontmatter
        let order = extractChapterNumber fullPath content
        
        -- Extract title and subsections from Pandoc AST
        pandoc <- runIO $ readMarkdown customReaderOptions (T.pack content)
        (title, subsections) <- case pandoc of
            Right doc@(Pandoc _ blocks) -> do
                let htmlName = replaceExtension fname ".html"
                    title = extractFirstHeading blocks
                    subsections = extractTOCFromPandoc htmlName doc
                return (title, subsections)
            Left err -> error $ "Failed to parse " ++ fullPath ++ ": " ++ show err
        
        -- Return just the filename, not the full path
        return (fname, order, title, subsections)
    
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
