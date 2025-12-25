{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
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

-- Data type for chapter metadata
data ChapterInfo = ChapterInfo
    { chapterFile :: FilePath
    , chapterNumber :: Int
    , chapterTitle :: String
    , chapterSections :: [Section]
    }

-- Data type for section with anchor and title
data Section = Section
    { sectionAnchor :: String
    , sectionTitle :: String
    }

-- Helper route to strip source_md/ directory and set .html extension
stripSourceMdRoute :: Routes
stripSourceMdRoute = customRoute (takeFileName . toFilePath) `composeRoutes` setExtension "html"

main :: IO ()
main = hakyll $ do
    -- Copy static assets managed by Hakyll
    match "static/**" $ do
        route $ gsubRoute "static/" (const "")
        compile copyFileCompiler
    
    -- Templates
    match "config/template.html" $ compile templateBodyCompiler
    match "config/chapters-toc.html" $ compile templateBodyCompiler
    
    -- Collect all chapters with their metadata
    chapterFiles <- buildChapterList
    
    -- Process chapter markdown files
    let chapterTriples = zipPrevNext chapterFiles
    forM_ chapterTriples $ \(mprev, ChapterInfo{chapterFile, chapterTitle}, mnext) -> do
        match (fromGlob $ "source_md" </> chapterFile) $ do
            route stripSourceMdRoute
            compile (customPandocCompiler
                >>= loadAndApplyTemplate "config/template.html" (chapterCtx mprev mnext chapterTitle))
    
    -- Generate chapters.html (TOC)
    create ["chapters.html"] $ do
        route idRoute
        compile $ do
            -- Build context with chapter list using structured section data
            -- Use a tuple to pass both chapter info and section to the context
            let sectionContext = 
                    field "link" (\item -> do
                        let (chFile, sec) = itemBody item
                        -- Build full URL from chapter file and section anchor
                        return $ replaceExtension chFile ".html" ++ "#" ++ sectionAnchor sec) <>
                    field "title" (return . sectionTitle . snd . itemBody)
                
                makeSectionItem :: ChapterInfo -> Section -> Item (FilePath, Section)
                makeSectionItem ch sec = Item (fromFilePath $ chapterFile ch) (chapterFile ch, sec)
                
                chapterItemContext = 
                    field "htmlname" (return . flip replaceExtension ".html" . chapterFile . itemBody) <>
                    field "title" (return . chapterTitle . itemBody) <>
                    field "number" (return . show . chapterNumber . itemBody) <>
                    listFieldWith "sections" sectionContext (\item -> 
                        let ch = itemBody item
                        in return $ map (makeSectionItem ch) $ chapterSections ch)
                
                makeChapterItem :: ChapterInfo -> Item ChapterInfo
                makeChapterItem ch = Item (fromFilePath $ chapterFile ch) ch
                
                chaptersCtx = 
                    listField "chapters" chapterItemContext (return $ map makeChapterItem chapterFiles) <>
                    constField "title" "Chapters - Learn You a Haskell for Great Good!" <>
                    defaultContext
            
            -- Use template to generate content
            makeItem ""
                >>= loadAndApplyTemplate "config/chapters-toc.html" chaptersCtx
                >>= loadAndApplyTemplate "config/template.html" chaptersCtx
    
    -- Generate faq.html
    match "source_md/faq.md" $ do
        route stripSourceMdRoute
        compile $ do
            customPandocCompiler
                >>= loadAndApplyTemplate "config/template.html"
                        (constField "title" "FAQ - Learn You a Haskell for Great Good!" <>
                         constField "faq" "true" <>
                         defaultContext)



-- Build list of chapters sorted by chapter number from YAML metadata
buildChapterList :: Rules [ChapterInfo]
buildChapterList = preprocess $ do
    files <- listDirectory "source_md"
    let mdFiles = filter (\f -> takeExtension f == ".md") files
        chapterFiles = filter (not . isFaqOrHelper) mdFiles
    chapters <- mapM getChapterData chapterFiles
    return $ sortOn chapterNumber chapters
  where
    getChapterData :: FilePath -> IO ChapterInfo
    getChapterData fname = do
        let fullPath = "source_md" </> fname
        content <- readFile fullPath
        
        -- Extract chapter number and other metadata from Pandoc's parsed metadata
        pandoc <- runIO $ readMarkdown customReaderOptions (T.pack content)
        (order, title, sections) <- case pandoc of
            Right pandocDoc@(Pandoc meta blocks) -> do
                let htmlName = replaceExtension fname ".html"
                    -- Helper to parse chapter number from MetaValue
                    parseChapterNumber :: T.Text -> Int
                    parseChapterNumber chapterStr = case reads (T.unpack chapterStr) of
                        [(n, "")] -> n
                        _ -> error $ "Invalid chapter number in " ++ fullPath
                    -- Extract chapter number from Pandoc metadata
                    order = case M.lookup "chapter" (unMeta meta) of
                        Just (MetaInlines [Str chapterStr]) -> parseChapterNumber chapterStr
                        Just (MetaString chapterStr) -> parseChapterNumber chapterStr
                        _ -> error $ "No chapter field in YAML metadata in: " ++ fullPath
                    title = extractFirstHeading blocks
                    sections = extractTOCFromPandoc htmlName pandocDoc
                return (order, title, sections)
            Left err -> error $ "Failed to parse " ++ fullPath ++ ": " ++ show err
        
        -- Return chapter info
        return ChapterInfo
            { chapterFile = fname
            , chapterNumber = order
            , chapterTitle = title
            , chapterSections = sections
            }
    
    isFaqOrHelper :: FilePath -> Bool
    isFaqOrHelper fname = "faq.md" `isInfixOf` fname
      where
        isInfixOf needle [] = False
        isInfixOf needle haystack@(x:xs)
          | needle `isPrefixOf` haystack = True
          | otherwise = isInfixOf needle xs

-- Helper function to build chapter context with optional prev/next navigation
chapterCtx :: Maybe ChapterInfo -> Maybe ChapterInfo -> String -> Context String
chapterCtx mprev mnext title =
    constField "title" title <>
    constField "footdiv" "true" <>
    maybeChapterContext "prev" mprev <>
    maybeChapterContext "next" mnext <>
    defaultContext
  where
    maybeChapterContext :: String -> Maybe ChapterInfo -> Context String
    maybeChapterContext prefix mchapter =
        maybe mempty (\ChapterInfo{chapterFile, chapterTitle} ->
            constField (prefix ++ "_filename") (replaceExtension chapterFile ".html") <>
            constField (prefix ++ "_title") chapterTitle) mchapter

-- Custom pandoc compiler that uses our custom reader options
customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWith customReaderOptions defaultHakyllWriterOptions

-- Custom reader options that disable implicit_figures extension
customReaderOptions :: ReaderOptions
customReaderOptions = defaultHakyllReaderOptions
  { readerExtensions = disableExtension Ext_implicit_figures 
                      (readerExtensions defaultHakyllReaderOptions)
  }

-- Extract first heading from Pandoc blocks
extractFirstHeading :: [Block] -> String
extractFirstHeading blocks = 
    case find isHeader blocks of
        Just (Header _ _ inlines) -> inlineToString inlines
        _ -> ""
  where
    isHeader (Header 1 _ _) = True
    isHeader _ = False

-- Extract TOC from Pandoc document using Pandoc's AST
extractTOCFromPandoc :: String -> Pandoc -> [Section]
extractTOCFromPandoc htmlName (Pandoc _ blocks) = query getSection blocks
  where
    getSection :: Block -> [Section]
    getSection (Header 2 (anchor, _, _) inlines) =
        [Section (T.unpack anchor) (inlineToString inlines)]
    getSection _ = []

-- Helper to convert Pandoc inlines to string
inlineToString :: [Inline] -> String
inlineToString = query getString
  where
    getString :: Inline -> String
    getString (Str s) = T.unpack s
    getString Space = " "
    getString (Code _ s) = T.unpack s
    getString _ = ""

-- Helper function to pair each element with its previous and next elements
zipPrevNext :: [a] -> [(Maybe a, a, Maybe a)]
zipPrevNext xs = zip3 (Nothing : map Just xs) xs (map Just (tail xs) ++ [Nothing])
