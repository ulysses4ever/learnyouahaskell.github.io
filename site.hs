{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.List (sortOn, find, isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query)
import Control.Monad (forM_)
import System.FilePath (takeBaseName)

main :: IO ()
main = hakyllWith config $ do
    -- Copy static assets managed by Hakyll
    match "docs/assets/**" $ do
        route $ gsubRoute "docs/" (const "")
        compile copyFileCompiler
    
    match "docs/sh/**" $ do
        route $ gsubRoute "docs/" (const "")
        compile copyFileCompiler
    
    match "docs/index.html" $ do
        route $ gsubRoute "docs/" (const "")
        compile copyFileCompiler
    
    match "docs/robots.txt" $ do
        route $ gsubRoute "docs/" (const "")
        compile copyFileCompiler
    
    match "docs/sitemap.xml" $ do
        route $ gsubRoute "docs/" (const "")
        compile copyFileCompiler
    
    -- Template
    match "markdown/config/template.html" $ compile templateBodyCompiler
    
    -- Collect all chapters with their metadata
    chapterFiles <- buildChapterList
    
    -- Process chapter markdown files
    forM_ chapterFiles $ \(fname, idx, title) -> do
        match (fromGlob $ "markdown/source_md/" ++ fname) $ do
            route $ customRoute $ \_ -> takeBaseName fname ++ ".html"
            compile $ do
                let total = length chapterFiles
                    
                    prevChapter = listToMaybe [(f, t) | (f, i, t) <- chapterFiles, i == idx - 1]
                    nextChapter = listToMaybe [(f, t) | (f, i, t) <- chapterFiles, i == idx + 1]
                    
                    (prevFile, prevTitle) = case prevChapter of
                        Just (f, t) -> (takeBaseName f, t)
                        Nothing -> ("", "")
                    
                    (nextFile, nextTitle) = case nextChapter of
                        Just (f, t) -> (takeBaseName f, t)
                        Nothing -> ("", "")
                
                pandocCompiler
                    >>= loadAndApplyTemplate "markdown/config/template.html" 
                            (chapterContext title prevFile prevTitle nextFile nextTitle)
                    >>= postProcessImages
    
    -- Generate chapters.html (TOC)
    create ["chapters.html"] $ do
        route idRoute
        compile $ do
            headContent <- unsafeCompiler $ readFile "markdown/source_md/chapters_head.md"
            footContent <- unsafeCompiler $ readFile "markdown/source_md/chapters_foot.md"
            
            -- Build TOC from all chapters using Pandoc to extract headings
            tocLines <- forM chapterFiles $ \(fname, idx, title) -> do
                let basename = takeBaseName fname
                    sp = if idx >= 10 then " " else "  "
                    chapterLine = show idx ++ "." ++ sp ++ "[" ++ title ++ "](" ++ basename ++ ".html)"
                
                -- Load chapter markdown, parse with Pandoc, extract subsections
                item <- load (fromFilePath $ "markdown/source_md/" ++ fname)
                pandoc <- readPandocWith defaultHakyllReaderOptions item
                let subsections = extractTOCFromPandoc basename (itemBody pandoc)
                
                return $ chapterLine : subsections
            
            let tocContent = unlines $ concat tocLines
                fullContent = headContent ++ "\n" ++ tocContent ++ "\n" ++ footContent
            
            makeItem fullContent
                >>= renderPandoc
                >>= loadAndApplyTemplate "markdown/config/template.html"
                        (constField "title" "Chapters - Learn You a Haskell for Great Good!" <>
                         defaultContext)
                >>= postProcessChaptersList
    
    -- Generate faq.html
    match "markdown/source_md/faq.md" $ do
        route $ constRoute "faq.html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "markdown/config/template.html"
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
    let pattern = "markdown/source_md/*.md"
    ids <- getMatches (fromGlob pattern)
    chapters <- mapM getChapterData $ filter (not . isFaqOrHelper . toFilePath) ids
    return $ sortOn (\(_, idx, _) -> idx) chapters
  where
    getChapterData :: Identifier -> IO (FilePath, Int, String)
    getChapterData ident = do
        let fname = toFilePath ident
        meta <- loadMetadata ident
        content <- readFile fname
        let order = fromMaybe 999 $ lookupInt "chapter" meta  -- Default to 999 if no metadata
            title = extractTitleFromContent content
        return (fname, order, title)
    
    isFaqOrHelper :: FilePath -> Bool
    isFaqOrHelper fname = any (`isInfixOf` fname) ["faq.md", "chapters_head.md", "chapters_foot.md"]
      where
        isInfixOf needle [] = False
        isInfixOf needle haystack@(x:xs)
          | needle `isPrefixOf` haystack = True
          | otherwise = isInfixOf needle xs

-- Extract title from markdown content
extractTitleFromContent :: String -> String
extractTitleFromContent content =
    let lns = dropWhile (\l -> l == "---" || null l) $ lines content
        titleLine = fromMaybe "" $ find (isPrefixOf "# ") lns
        title = drop 2 titleLine
        cleanTitle = T.unpack $ T.strip $ T.pack $ takeWhile (/= '{') title
    in cleanTitle

-- Extract TOC from Pandoc document using Pandoc's AST
extractTOCFromPandoc :: String -> Pandoc -> [String]
extractTOCFromPandoc basename (Pandoc _ blocks) =
    let headers = query getHeader blocks
    in mapMaybe (makeLink basename) headers
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
    makeLink base (level, anchor, title)
        | level == 2 = Just $ "    * [" ++ title ++ "](" ++ base ++ ".html#" ++ anchor ++ ")"
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
