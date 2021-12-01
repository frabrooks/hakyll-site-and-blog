--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
import qualified GHC.IO.Encoding as E
import GHC.Generics

import           Hakyll
import           Hakyll.Core.Compiler.Internal (compilerUnderlying, compilerAsk)
import           Hakyll.Web.Sass (sassCompiler)
import           Data.Char (isSpace, toLower)
import           Data.List (sort)
import           Data.Maybe (fromJust, fromMaybe, isJust)
import           System.FilePath (takeFileName, takeBaseName, (</>), (<.>), dropExtension, takeDirectory, equalFilePath)
import           System.Directory (doesFileExist)

import           Text.Pandoc.Options
import           Text.Pandoc.Extensions
import           Text.Pandoc.Highlighting (Style, breezeDark, espresso, tango, styleToCss)


import Control.Monad.Loops
import Data.Text(pack, unpack, replace)

import Data.Typeable
import Data.Binary
import Hakyll.Core.Writable

-- Local Lib Import
import Parsing

data CompiledBlogPost = CompiledBlogPost String BlogPostMetaData deriving (Show, Generic) 

deriving instance Binary BlogPostMetaData
deriving instance Binary CompiledBlogPost

instance Writable CompiledBlogPost where

  write fp (Item i (CompiledBlogPost s _)) = write fp (Item i s)

data ThumbType = JPG | PNG


-- Constants
numberOfRecentActivitiesToDisplay = 4

--------------------------------------------------------------------------------
main :: IO ()
main = do

  E.setLocaleEncoding E.utf8
  
  putStrLn "Parsing 'content/open-source-journal.txt'"
  maybeJrnl <- safeParseFile "content/open-source-journal.txt" openSrcJrnlP
  let openSrcJrnl = fromJust maybeJrnl
  
  let jrnlEntries = jrnlItems openSrcJrnl
  
  putStrLn "Parsing 'about-me.txt'"
  maybeAboutMe <- safeParseFile "content/about-me.txt" aboutMeP
  let aboutMe = fromJust maybeAboutMe
 
  putStrLn "Parsing 'content/academic-work/'"
  academicWork <- getAcademicWork

  putStrLn "Parsing 'content/project-portfolio/'"
  projects <- getProjects
  
  hakyll $ do

    match "index.html" $ do
      route idRoute
      compile $ do

        posts <- loadAll (fromRegex "content/posts/.*(.tex|.md|.txt|.html)") :: Compiler [Item CompiledBlogPost]
        let postMetas = fmap (getMeta . itemBody) posts
        
        let recentActivities = ((take numberOfRecentActivitiesToDisplay) . reverse . sort)
                               $ combineAsActivities jrnlEntries postMetas
        
        let activityCtx =
              field "activity-date" (return . getTimeFormatter . activityDate . itemBody) <>
              field "activity-title" (return . activityTitle . itemBody) <>
              field "link" (return . linkToItem . itemBody) <>
              boolField "is-post" (\ (Item _ a) ->
                                     case (itemType a) of Post -> True ; _ -> False) <>
              boolField "is-merge" (\ (Item _ a) ->
                                      case (itemType a) of Merge -> True ; _ -> False)

        let portfolioItemCtx =
              field "item-title" (return . itemTitle . itemBody) <>
              field "item-link" (return . itemLink . itemBody) <>
              field "item-image" (return . itemImgSrc . itemBody) <>
              listFieldWith "item-pars" (bodyField "body")
                (\ (Item _ a) -> sequence $ fmap makeItem (itemDesc a)) <>
              listFieldWith "item-tags" (bodyField "body")
                (\ (Item _ a) -> sequence $ fmap makeItem (itemTags a)) <>
              boolField "github" (\ (Item _ a) ->
                                    case (gitHubLink a) of Just _ -> True ; _ -> False) <>
              boolField "pdf" (\ (Item _ a) ->
                                 case (pdfLink a) of Just _ -> True ; _ -> False) <>
              field "github-link" (return . fromJust . gitHubLink . itemBody) <>
              field "pdf-link" (return . fromJust . pdfLink . itemBody)
        
        let indexCtx =
              constField "email" (email aboutMe) <>
              constField "first-name" (firstName aboutMe) <>
              constField "last-name" (lastName aboutMe) <>
              constField "subtitleLHS" (subtitleLHS aboutMe) <>
              constField "subtitleRHS" (subtitleRHS aboutMe) <>
              listField "about-me-pars" (bodyField "body")
                (sequence $ fmap makeItem (aboutMePars aboutMe)) <>
              listField "recent-activity-list" activityCtx
                (sequence $ fmap makeItem recentActivities) <>
              listField "academic-work" portfolioItemCtx
                (sequence $ fmap makeItem academicWork) <>
              listField "projects" portfolioItemCtx
                (sequence $ fmap makeItem projects)

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/root.html" defaultContext
          >>= relativizeUrls
    
    match "open-source-jrnl.html" $ do
      route idRoute
      compile $ do
        
        let jrnlEntryCtx =
              field "srcRepo"
                    (return . (replaceString "➔" "⮯") . head . (splitAfter '➔') . srcTitle . itemBody) <>
              boolField "hasRepo"
                        ((> 0) . length .  head . (splitAfter '➔') . srcTitle . itemBody) <>
              boolField "isMerge" (\ i -> case (srcType $ itemBody i) of Merge -> True ; _ -> False) <>
              field "srcTitle" (return . head . tail . (splitAfter '➔') . srcTitle . itemBody) <>
              field "srcDate" (return . srcDate . itemBody) <>
              field "linesAdded" (return . linesAdded . itemBody) <>
              field "linesRemoved" (return . linesRemoved . itemBody) <>
              field "srcLink" (return . srcLink . itemBody) <>
              listFieldWith "srcDescription" (bodyField "body")
                (\ (Item _ a) -> sequence $ fmap makeItem (srcDescription a)) <>
              listFieldWith "srcTags" (bodyField "body")
                (\ (Item _ a) -> sequence $ fmap makeItem (srcTags a))
        let jrnlCtx =
              listFieldWith "jrnlIntro" (bodyField "body")
                (\ (Item _ a) -> sequence $ fmap makeItem (jrnlIntro openSrcJrnl)) <>
              listField "jrnlEntries" jrnlEntryCtx (sequence $ fmap makeItem jrnlEntries)

        getResourceBody
          >>= applyAsTemplate jrnlCtx
          >>= loadAndApplyTemplate "templates/root.html" defaultContext
          >>= relativizeUrls        

    match "blog.html" $ do
      route idRoute
      compile $ do

        let posts = loadAll (fromRegex "content/posts/.*(.tex|.md|.txt|.html)") :: Compiler [Item CompiledBlogPost]

        postsDesc <- unsafeCompiler (unsafeParseFile (inWS parsP) "./content/posts-index.txt")

        let blogCtx =
              listFieldWith "blogIntro" (bodyField "body")
                (\ (Item _ a) -> sequence $ fmap makeItem postsDesc) <>
              listField "postsList" postMetaCtx posts 

        getResourceBody
          >>= applyAsTemplate blogCtx
          >>= loadAndApplyTemplate "templates/root.html" (constField "title" "Blog" <> defaultContext)
          >>= relativizeUrls
    
    match "js/*" $ do
      route idRoute
      compile copyFileCompiler

    match "fonts/*" $ do
      route idRoute
      compile copyFileCompiler

  
    match "content/images/*" $ do
        route  $ customRoute $ (((</>) "./images/") . takeFileName . toFilePath)
        compile copyFileCompiler

    match "content/cv/cv.pdf" $ do
      route $ customRoute $ (\ _ -> "content/pdfs/cv.pdf")
      compile copyFileCompiler
        
    match "content/pdfs/*" $ do
      route   idRoute
      compile copyFileCompiler

    match (fromRegex "scss/[^_]*.scss") $ do
      route $ customRoute $ (\ id -> "./css/" </> (takeBaseName $ toFilePath id) <.> ".css")
      compile sassCompiler
      
    match (fromList ["about.rst", "contact.markdown"]) $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/root.html" defaultContext
        >>= relativizeUrls

    -- Load and compile all blog posts
    match (fromRegex "content/posts/.*(.tex|.md|.txt|.html)") $ do
      
      route  $ metadataRoute (\ hakyllMetaData -> case lookupString "title" hakyllMetaData of
                                 Nothing -> undefined
                                 Just title -> customRoute $ \ identifier ->
                                   pathFromDateAndTitle (dateFromFilePath (toFilePath identifier)) title
                             )

      compile $ do

        identifier <- getUnderlying
        hakyllMetaData <- getMetadata identifier
        let postFP  = toFilePath identifier -- = content/posts/...
        let postDir = takeDirectory postFP -- = content/posts/...


        unsafeCompiler $ putStrLn ("fp is: " ++  (toFilePath identifier))
        
        let title = fromJust (lookupString "title" hakyllMetaData)
        let date = dateFromFilePath (toFilePath identifier)
        let isTechnical = isJust (lookupString "technical" hakyllMetaData)
        let sitePath = pathFromDateAndTitle date title


        -- = JPG or PNG
        thumbType <- unsafeCompiler (searchForThumbnail postDir)
        -- = absolute, i.e.   /blog/$DATE$-$TITLE$/thumb.jpg
        let thumbnail = fmap (thumbnailFromPath sitePath) thumbType

 
      --  unsafeCompiler $ putStrLn ("isTech == " ++ (show isTechnical))
        
        let tags = fromMaybe [] (lookupStringList "tags" hakyllMetaData)        

        imgPaths <- unsafeCompiler getBlogImgPaths

        let rawImgPaths' = filter ((equalFilePath postDir) . takeDirectory) imgPaths
        let imgFileNames = fmap takeFileName rawImgPaths'
        let compiledImgPaths = fmap (\ fp -> (dropExtension sitePath) </> (takeFileName fp)) rawImgPaths'

        -- Make all paths absolute to project root (by dropping the '.' in ./blog/...)
        -- The relaivizeUrls below should handle pages shifting around, not that they should be doing so
        let absCompiledImgPaths = fmap tail compiledImgPaths

        
        let imgFileNamesWithCompiledPaths
              = zipWith (\ a b -> ("src=\"" ++ a ++ "\"", "src=\"" ++ b ++ "\"")) imgFileNames absCompiledImgPaths

        unsafeCompiler $ putStrLn (show imgFileNamesWithCompiledPaths)
        
        compiled <- pandocMathCompiler
          >>= loadAndApplyTemplate "templates/post.html" (postCtx thumbnail isTechnical)
          >>= loadAndApplyTemplate "templates/root.html" rootBlogCtx
          >>= doReplaceAll imgFileNamesWithCompiledPaths
          >>= relativizeUrls


        -- Return all meta information paired and compiled post wrapped in CompiledBlogPost
        let compiledPost = itemBody compiled
        let compiledFirstPar = unsafeParse firstBodyParP compiledPost

        let mDesc = lookupString "desc" hakyllMetaData        
        let desc = fromMaybe compiledFirstPar mDesc
        
        let postMeta = BlogPostMetaData title date sitePath isTechnical desc thumbnail tags        
        makeItem $ CompiledBlogPost (itemBody compiled) postMeta


    match (fromRegex "content/posts/.*(.jpg|.png|.gif)") $ do

      postPaths <- preprocess getBlogPostPaths
      contents <- preprocess $ sequence $ fmap readFile postPaths
      let metas = fmap (unsafeParse getMetaP) contents
      let postDirs = fmap takeDirectory postPaths
      let postTitles = fmap (unsafeParse getTitleP) metas
      let dirsWTitles = zipWith (,) postDirs postTitles
      
      route $ customRoute $ (\ identifier ->
                               let comp = (\ p -> equalFilePath (fst p) (takeDirectory (toFilePath identifier)))
                               in
                               let title = (snd . head) $ filter comp dirsWTitles
                               in
                               let path = pathFromDateAndTitle (dateFromFilePath (toFilePath identifier)) title
                               in
                               (dropExtension path) </> (takeFileName (toFilePath identifier)) )
                               
      compile copyFileCompiler

    -- Syntax styling used by blog posts with code
    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do
        makeItem $ styleToCss pandocCodeStyle


    match (fromRegex "content/academic-work/.*(.jpg|.png|.gif)") $ do
      route idRoute
      compile copyFileCompiler

    match (fromRegex "content/project-portfolio/.*(.jpg|.png|.gif)") $ do
      route idRoute
      compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler

        

--------------------------------------------------------------------------------

-- For rendering/compiling post page itself
postCtx :: Maybe FilePath -> Bool -> Context String
postCtx maybeThumbnail isTechnical =
  listContextWith (bodyField "tag") "tags" <>
  dateField "date" "%F" <>
  constField "thumbnailPath" (fromMaybe "error" maybeThumbnail) <>
  boolField "hasThumbnail" (\ _ -> isJust maybeThumbnail) <>
  boolField "technical" (\ _ -> isTechnical) <>
  defaultContext


rootBlogCtx :: Context String
rootBlogCtx = constField "title" "Blog" <>
              boolField "isBlogPost" (\ _ -> True) <>
              defaultContext

postMetaCtx :: Context CompiledBlogPost
postMetaCtx =
  field "postTitle" (return . postTitle . getMeta . itemBody) <>
  field "postDate" (return . postDate . getMeta . itemBody) <>
  field "postLink" (return . postLink . getMeta . itemBody) <>
  field "postDesc" (return . postDesc . getMeta . itemBody) <>
  boolField "technical" (isTechnical . getMeta . itemBody) <>
  listFieldWith "postTags" (bodyField "body")
  (\ (Item _ a) -> sequence $ fmap makeItem (postTags (getMeta a))) <>
  boolField "hasThumbnail" (isJust . thumbnail . getMeta . itemBody) <>
  field "imgSrc" (return . (fromMaybe "error") . thumbnail . getMeta . itemBody)  


searchForThumbnail :: FilePath -> IO (Maybe ThumbType)
searchForThumbnail dir = do
  let png = (dir </> "thumb.png")
  let jpg = (dir </> "thumb.jpg")
  bPNG <- doesFileExist png
  bJPG <- doesFileExist jpg
  case bPNG of
    True -> return $ Just PNG
    _ -> case bJPG of
      True -> return $ Just JPG
      _ -> return Nothing



dateFromFilePath :: FilePath -> String
dateFromFilePath fp = unsafeParse getDateP fp


-- | Get the compiled FilePath from a Date and Title of a BlogPost
--   i.e. content/posts/%DATE%...  goes to blog/%DATE$-$formatted-title$.html
pathFromDateAndTitle :: String -> String -> FilePath
pathFromDateAndTitle date title =
  let title' = ((fmap toLower) . (replaceString "/" "-") . (replaceString " " "-")) title
  in "./blog/" </> (date ++ "-" ++ title') <.> ".html"


-- | Get the thumbnail, and drop the relative part turning ./blog/... into
--   /blog/... 
thumbnailFromPath :: FilePath -> ThumbType -> FilePath
thumbnailFromPath fp JPG = tail $ (dropExtension fp) </> "thumb.jpg"
thumbnailFromPath fp PNG = tail $ (dropExtension fp) </> "thumb.png"


listContextWith :: Context String -> String -> Context a
listContextWith ctx s = listField s ctx $ do
    identifier <- getUnderlying
    metadata <- getMetadata identifier
    let metas = fromMaybe [] (lookupStringList s metadata)
--    let metas = maybe [] (map trim . splitAll ",") $ M.lookup s metadata
    return $ map (\ x -> Item (fromFilePath x) x) metas


pandocCodeStyle :: Style
pandocCodeStyle = espresso

pandocMathWriterOptions :: WriterOptions
pandocMathWriterOptions = let
  mathExtensions = extensionsFromList [Ext_tex_math_dollars,
                                       Ext_tex_math_double_backslash,
                                       Ext_latex_macros,
                                       Ext_raw_tex]
  defaultExtensions = writerExtensions defaultHakyllWriterOptions
  newExtensions = defaultExtensions <> mathExtensions
  in defaultHakyllWriterOptions {
                        writerExtensions = newExtensions
                        ,
                        writerHTMLMathMethod = MathJax ""
                        ,
                        writerHighlightStyle = Just pandocCodeStyle
                        }

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler = pandocCompilerWith defaultHakyllReaderOptions pandocMathWriterOptions


clipDesc :: String -> String
clipDesc s = let s' = take 195 s in if (length s' == 195) then f s' else s'
  where
    f = ((flip (++)) "...") . reverse . tail . (dropWhile (not . isSpace))  . reverse


splitAfter :: Char -> String -> [String]
splitAfter c xs = aux c "" xs
  where
    aux c ys "" = [[], ys]
    aux c ys (x : xs)
      | c == x = [ys ++ [x], xs]
      | otherwise = aux c (ys ++ [x]) xs


getMeta :: CompiledBlogPost -> BlogPostMetaData
getMeta (CompiledBlogPost _ m) = m


-- replace url of the form foo/bar/index.html by foo/bar
replaceIn :: (String, String) -> Item String -> Compiler (Item String)
replaceIn (s, with) item = return $ fmap (replaceString s with) item

doReplaceAll :: [(String, String)] -> Item String -> Compiler (Item String)
doReplaceAll ls = concatM (fmap replaceIn ls)

replaceString :: String -> String -> String -> String
replaceString string with = unpack . replace (pack string) (pack with) . pack

                                 
