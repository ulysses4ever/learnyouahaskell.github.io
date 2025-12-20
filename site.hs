{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.List (isPrefixOf, find, isSuffixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.Char (isSpace)
import Control.Monad (forM_)

main :: IO ()
main = hakyllWith config $ do
    -- Template
    match "markdown/config/template.html" $ compile templateBodyCompiler
    
    -- Read the chapter file list
    chapterFiles <- preprocess $ do
        fileListContent <- readFile "markdown/config/file-list.txt"
        return $ filter (not . null) $ lines fileListContent
    
    -- Pre-load all titles
    titles <- preprocess $ mapM extractTitle chapterFiles
    let chapterMap = M.fromList $ zip chapterFiles (zip [1..] titles)
    
    -- Process chapter markdown files
    forM_ chapterFiles $ \fname -> do
        match (fromGlob $ "markdown/source_md/" ++ fname ++ ".md") $ do
            route $ constRoute $ fname ++ ".html"
            compile $ do
                let Just (idx, title) = M.lookup fname chapterMap
                    total = length chapterFiles
                    
                    (prevFile, prevTitle) = if idx > 1
                        then let pf = chapterFiles !! (idx - 2)
                                 Just (_, pt) = M.lookup pf chapterMap
                             in (pf, pt)
                        else ("", "")
                    
                    (nextFile, nextTitle) = if idx < total
                        then let nf = chapterFiles !! idx
                                 Just (_, nt) = M.lookup nf chapterMap
                             in (nf, nt)
                        else ("", "")
                
                pandocCompiler
                    >>= loadAndApplyTemplate "markdown/config/template.html" 
                            (chapterContext title title prevFile prevTitle nextFile nextTitle)
                    >>= postProcessImages
    
    -- Generate chapters.html (TOC)
    tocEntries <- preprocess $ mapM (extractTocForFile chapterFiles chapterMap) (zip [1..] chapterFiles)
    let tocContent = unlines (concat tocEntries)
    headContent <- preprocess $ readFile "markdown/source_md/chapters_head.md"
    footContent <- preprocess $ readFile "markdown/source_md/chapters_foot.md"
    let fullContent = headContent ++ "\n" ++ tocContent ++ "\n" ++ footContent
    
    create ["chapters.html"] $ do
        route idRoute
        compile $ do
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
    { destinationDirectory = "docs"
    , storeDirectory = "_hakyll_cache"
    , tmpDirectory = "_hakyll_tmp"
    , providerDirectory = "."
    }

-- Extract title from markdown file (first # heading)
extractTitle :: FilePath -> IO String
extractTitle fname = do
    content <- readFile ("markdown/source_md/" ++ fname ++ ".md")
    let titleLine = fromMaybe "" $ find (isPrefixOf "# ") $ lines content
        title = drop 2 titleLine  -- Remove "# "
        cleanTitle = trim $ takeWhile (/= '{') title  -- Remove {#anchor} if present
    return cleanTitle
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Extract TOC entries for a file (chapter + subsections)
extractTocForFile :: [FilePath] -> M.Map FilePath (Int, String) -> (Int, FilePath) -> IO [String]
extractTocForFile allFiles chapterMap (num, fname) = do
    content <- readFile ("markdown/source_md/" ++ fname ++ ".md")
    let Just (_, title) = M.lookup fname chapterMap
        sp = if num >= 10 then " " else "  "
        chapterLine = show num ++ "." ++ sp ++ "[" ++ title ++ "](" ++ fname ++ ".html)"
        
        -- Extract subsections (## headings with anchors)
        subsections = extractSubsections fname content
    
    return (chapterLine : subsections)

-- Extract subsection links from markdown
extractSubsections :: FilePath -> String -> [String]
extractSubsections fname content =
    let lns = lines content
        subsecLines = filter (isPrefixOf "## ") lns
        makeLink line = 
            let titleRaw = trim $ drop 3 line
                title = takeWhile (/= '{') titleRaw
                anchor = extractAnchor line
            in case anchor of
                Just anch -> "    * [" ++ trim title ++ "](" ++ fname ++ ".html#" ++ anch ++ ")"
                Nothing -> ""
        links = map makeLink subsecLines
    in filter (not . null) links
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    -- Extract anchor from {#anchor} syntax
    extractAnchor line =
        case break (== '{') line of
            (_, "") -> Nothing
            (_, rest) -> 
                let anchor = takeWhile (/= '}') (drop 2 rest)  -- Skip "{#"
                in if "#" `isPrefixOf` drop 1 rest && not (null anchor)
                   then Just anchor
                   else Nothing

-- Context for chapter pages  
chapterContext :: String -> String -> FilePath -> String -> FilePath -> String -> Context String
chapterContext pageTitle title prevFile prevTitle nextFile nextTitle =
    constField "title" pageTitle <>
    constField "pagetitle" title <>
    constField "footdiv" "true" <>
    constField "prev_filename" prevFile <>
    constField "prev_title" prevTitle <>
    constField "next_filename" nextFile <>
    constField "next_title" nextTitle <>
    defaultContext

-- Post-process images (replicate sed command behavior)
postProcessImages :: Item String -> Compiler (Item String)
postProcessImages item = return $ fmap processHtml item
  where
    processHtml html = 
        let -- Simple replacement:  /> -> >
            step1 = replace " />" ">" html
        in step1
    replace old new str =
        case breakOn old str of
            (before, "") -> before
            (before, rest) -> before ++ new ++ replace old new (drop (length old) rest)
    breakOn needle haystack = go needle haystack []
      where
        go ndl [] acc = (reverse acc, [])
        go ndl hstk acc
            | ndl `isPrefixOf` hstk = (reverse acc, hstk)
            | otherwise = go ndl (tail hstk) (head hstk : acc)

-- Post-process chapters list (add class to ol)
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
            | otherwise = go ndl (tail hstk) (head hstk : acc)
