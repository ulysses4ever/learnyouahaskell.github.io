{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
import Hakyll
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, catMaybes)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Class (runIO, runIOorExplode)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Options (def, readerExtensions, writerExtensions, Extension(Ext_implicit_figures), ReaderOptions)
import Text.Pandoc.Extensions (disableExtension)
import System.Directory (listDirectory)
import Control.Monad (forM_, forM)
import System.FilePath ((</>), replaceExtension, takeBaseName, takeExtension, takeFileName)

-- Directory paths
sourceMdDir :: FilePath
sourceMdDir = "source_md"

templatesDir :: FilePath
templatesDir = "templates"

staticDir :: FilePath
staticDir = "static"

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
    match (fromGlob $ staticDir </> "**") $ do
        route $ gsubRoute (staticDir ++ "/") (const "")
        compile copyFileCompiler
    
    -- Templates
    match (fromGlob $ templatesDir </> "template.html") $ compile templateBodyCompiler
    match (fromGlob $ templatesDir </> "chapters-toc.html") $ compile templateBodyCompiler
    
    -- Collect all chapters with their metadata
    chapterFiles <- buildChapterList
    
    -- Process chapter markdown files
    let chapterTriples = zipPrevNext chapterFiles
    forM_ chapterTriples $ \(mprev, ChapterInfo{chapterFile}, mnext) -> do
        match (fromGlob $ sourceMdDir </> chapterFile) $ do
            route stripSourceMdRoute
            compile (customPandocCompiler
                >>= loadAndApplyTemplate (fromFilePath $ templatesDir </> "template.html") (chapterCtx mprev mnext))
    
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
                    defaultContext
            
            -- Use template to generate content with YAML frontmatter for title
            makeItem "---\ntitle: \"Chapters - Learn You a Haskell for Great Good!\"\n---\n"
                >>= loadAndApplyTemplate (fromFilePath $ templatesDir </> "chapters-toc.html") chaptersCtx
                >>= loadAndApplyTemplate (fromFilePath $ templatesDir </> "template.html") chaptersCtx
    
    -- Generate faq.html
    match (fromGlob $ sourceMdDir </> "faq.md") $ do
        route stripSourceMdRoute
        compile $ do
            customPandocCompiler
                >>= loadAndApplyTemplate (fromFilePath $ templatesDir </> "template.html")
                        (constField "faq" "true" <>
                         defaultContext)



-- Build list of chapters sorted by chapter number from YAML metadata
buildChapterList :: Rules [ChapterInfo]
buildChapterList = preprocess $ do
    files <- listDirectory sourceMdDir
    maybeChapters <- mapM getChapterData files
    return $ sortOn chapterNumber (catMaybes maybeChapters)
  where
    getChapterData :: FilePath -> IO (Maybe ChapterInfo)
    getChapterData fname = do
        let fullPath = sourceMdDir </> fname
        content <- readFile fullPath
        
        -- Extract chapter number and other metadata from Pandoc's parsed metadata
        pandoc <- runIO $ readMarkdown customReaderOptions (T.pack content)
        case pandoc of
            Right pandocDoc@(Pandoc meta blocks) -> do
                -- Check if this file has a chapter number in metadata
                case M.lookup "chapter" (unMeta meta) of
                    Just (MetaString chapterStr) -> do
                        let htmlName = replaceExtension fname ".html"
                            -- Parse chapter number from MetaValue
                            order = case reads (T.unpack chapterStr) of
                                [(n, "")] -> n
                                _ -> error $ "Invalid chapter number in " ++ fullPath
                            -- Extract title from metadata
                            title = case M.lookup "title" (unMeta meta) of
                                Just (MetaInlines inlines) -> T.unpack $ stringify inlines
                                Just (MetaString s) -> T.unpack s
                                _ -> error $ "No title found in " ++ fullPath
                            sections = extractTOCFromPandoc htmlName pandocDoc
                        return $ Just ChapterInfo
                            { chapterFile = fname
                            , chapterNumber = order
                            , chapterTitle = title
                            , chapterSections = sections
                            }
                    _ -> return Nothing  -- Not a chapter file (e.g., FAQ)
            Left err -> error $ "Failed to parse " ++ fullPath ++ ": " ++ show err

-- Helper function to build chapter context with optional prev/next navigation
chapterCtx :: Maybe ChapterInfo -> Maybe ChapterInfo -> Context String
chapterCtx mprev mnext =
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

-- Extract TOC from Pandoc document using Pandoc's AST
extractTOCFromPandoc :: String -> Pandoc -> [Section]
extractTOCFromPandoc htmlName (Pandoc _ blocks) = query getSection blocks
  where
    getSection :: Block -> [Section]
    getSection (Header 1 (anchor, _, _) inlines) =
        [Section (T.unpack anchor) (T.unpack $ stringify inlines)]
    getSection _ = []

-- Helper function to pair each element with its previous and next elements
zipPrevNext :: [a] -> [(Maybe a, a, Maybe a)]
zipPrevNext xs = zip3 (Nothing : map Just xs) xs (map Just (tail xs) ++ [Nothing])
