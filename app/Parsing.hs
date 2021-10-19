
module Parsing ( module SiteParser
               , module BasicParsers
               , module SiteData
               , module SiteDataParsers
               , unsafeParseFile
               , unsafeParseFile'
               , unsafeRunParse
               , unsafeRunParse'
               , unsafeParse
               , safeParseFile
               , safeRunParse
               , getAcademicWork
               , getProjects
               , getBlogPostPaths
               , getBlogImgPaths
               ) where


-- Lib Imports
import Data.Char (toLower)
import Data.List (intercalate, sort, zipWith7)
import Data.Maybe (fromJust)
import Data.Text (pack, isInfixOf)
import System.FilePath
import qualified System.FilePath.Find as SFF
import System.Directory


-- Local Imports
import SiteParser
import BasicParsers
import SiteData
import SiteDataParsers


unsafeParseFile :: Parser a -> FilePath -> IO a
unsafeParseFile parser filepath = do
  input <- readFile filepath
  case runWriter $ runMaybeT (runParser parser input) of
    (Nothing, err) -> do
      putStrLn "Parse Failed: Printing Log..."
      putStrLn $ intercalate "\n" err
      putStrLn "Entering Undefined"
      undefined
    (Just (rest, x), _) -> do
      return x

unsafeParseFile' :: Parser a -> FilePath -> IO (String, a)
unsafeParseFile' parser filepath = do
  input <- readFile filepath
  case runWriter $ runMaybeT (runParser parser input) of
    (Nothing, err) -> do
      putStrLn "Parse Failed: Printing Log..."
      putStrLn $ intercalate "\n" err
      putStrLn "Entering Undefined"
      undefined
    (Just (rest, x), _) -> do
      return (rest, x)


unsafeParse :: Parser a -> String -> a
unsafeParse parser input = 
  case runWriter $ runMaybeT (runParser parser input) of
    (Nothing, err) -> undefined
    (Just (rest, a), _) -> a


unsafeRunParse :: Parser a -> String -> IO a
unsafeRunParse parser input = do
  case runWriter $ runMaybeT (runParser parser input) of
    (Nothing, err) -> do
      putStrLn "Parse Failed: Printing Log..."
      putStrLn $ intercalate "\n" err
      putStrLn "Entering Undefined"
      undefined
    (Just (rest, x), _) -> do
      return x

unsafeRunParse' :: Parser a -> String -> IO (String, a)
unsafeRunParse' parser input = do
  case runWriter $ runMaybeT (runParser parser input) of
    (Nothing, err) -> do
      putStrLn "Parse Failed: Printing Log..."
      putStrLn $ intercalate "\n" err
      putStrLn "Entering Undefined"
      undefined
    (Just (rest, x), _) -> do
      return (rest, x)


safeParseFile :: (Show a) => FilePath -> Parser a -> IO (Maybe a)
safeParseFile filepath parser = do
  input <- readFile filepath
  case runWriter $ runMaybeT (runParser parser input) of
    (Nothing, err) -> do
      putStrLn "Parse Failed: Printing Log..."
      putStrLn $ intercalate "\n" err
      return Nothing
    (Just ("", x), _) -> do
      putStrLn "Parse finished successfully"
      putStrLn "::"
      putStrLn (show x)
      putStrLn "::"
      return (Just x)
    (Just (str, x), err) -> do
      putStrLn "Waring! Some input still remaining. Printing Log..."
      putStrLn $ intercalate "\n" err
      putStrLn "Printing remaining input:"
      putStrLn str
      return (Just x)


safeRunParse :: Parser a -> String -> IO (Maybe a)
safeRunParse parser input = do
  case runWriter $ runMaybeT (runParser parser input) of
    (Nothing, _) -> return Nothing
    (Just (_, s), _) -> return (Just s)
  

-- | Portfolio Item Parser
getPortfolioItems :: String -> IO [PortfolioItem]
getPortfolioItems dir = do
  files <- listDirectory dir
  let items = sort $ filter (\ fp -> (not . (isInfixOf $ pack ".jpg") . pack) fp
                             && (not . (isInfixOf $ pack ".png") . pack) fp) files
  items' <- sequence $ fmap (jpgORpng . ((</>) dir)) items
  let items'' = zip items items'
  sequence $ fmap (\ item ->
                      unsafeParseFile
                        (portfolioItemP (snd item))
                        (dir </> (fst item))
                  ) items''


-- | AcademicWorkList Parser
getAcademicWork :: IO (AcademicWorkList)
getAcademicWork = getPortfolioItems "./content/academic-work/"


-- | ProjectPortfolio Parser
getProjects :: IO (ProjectPortfolio)
getProjects = getPortfolioItems "./content/project-portfolio/"


--toSitePath :: FilePath -> IO FilePath
--toSitePath fp = do
--  date <- (unsafeRunParse getDateP) fp
--  contents <- readFile fp
--  meta <- (unsafeRunParse getMetaP) contents
--  title <- (unsafeRunParse getTitleP) meta
--  let formatTitle = (fmap toLower) . (replace '/' '-') . (replace ' ' '-')
--  return $ "./blog/" </> date ++ (formatTitle title) <.> ".html"


--doPostsMetaParse :: IO [BlogPostMetaData]
--doPostsMetaParse = do
--  paths <- getBlogPostPaths
--  paths' <- sequence $ fmap toSitePath paths
--  contents <- sequence $ fmap readFile paths
--  meta <- sequence $ fmap (unsafeRunParse getMetaP) contents
--  titles   <- sequence $ fmap (unsafeRunParse getTitleP) meta
--  dates <- sequence $ fmap (unsafeRunParse getDateP) paths
--  let dates' = fmap (reverse . tail . reverse) dates
--  cats <- sequence $ fmap (unsafeRunParse getCatP) meta
--  tags <- sequence $ fmap (unsafeRunParse getTagsP) meta
--  -- Maybe description, if Nothing then desc comes from
--  -- parsing with firstBodyParP for first <p>...</p>
--  mDescs <- sequence $ fmap (safeRunParse getDescP) meta
--  let splits = fmap splitFileName paths
--  let dirs = fmap fst splits
--  let postFiles = fmap snd splits
--  let jpgPaths = fmap (flip (</>) $ "thumb.jpg") dirs
--  bJpg <- sequence $ fmap doesFileExist jpgPaths
--  let jpgPaths' = zip bJpg jpgPaths
--  let pngPaths = fmap (flip (</>) $ "thumb.png") dirs
--  bPng <- sequence $ fmap doesFileExist pngPaths
--  let pngPaths' = zip bPng pngPaths
--  let mThumbnails = zipWith (\ jpg png -> if (fst png)
--                                         then Just (snd png)
--                                         else if (fst jpg)
--                                              then Just (snd jpg)
--                                              else Nothing) jpgPaths' pngPaths'
--  putStrLn $
--    "\n\n Parsed Blog Post meta data:\n\n" ++
--    intercalate "\n\n" (fmap show $
--      zipWith7 BlogPostMetaData titles dates' paths' cats mDescs mThumbnails tags)
--    ++ "\n\n"
--  return $ zipWith7 BlogPostMetaData titles dates' paths' cats mDescs mThumbnails tags


notHidden :: SFF.FindClause Bool
notHidden = SFF.fileName SFF./~? ".?*"

isBlogPost :: SFF.FindClause Bool
isBlogPost = SFF.fileName SFF.~~? "*.tex"
             SFF.||?
             SFF.fileName SFF.~~? "*.md"
             SFF.||?
             SFF.fileName SFF.~~? "*.txt"
             SFF.||?
             SFF.fileName SFF.~~? "*.html"


getBlogPostPaths :: IO [ FilePath ] -- notHidden = don't search hidden dirs
getBlogPostPaths = SFF.find notHidden isBlogPost "./content/posts/"



isImg :: SFF.FindClause Bool
isImg = SFF.fileName SFF.~~? "*.png"
        SFF.||?
        SFF.fileName SFF.~~? "*.jpg"
        SFF.||?
        SFF.fileName SFF.~~? "*.gif"

getBlogImgPaths :: IO [ FilePath ]
getBlogImgPaths = SFF.find notHidden isImg "./content/posts/"






