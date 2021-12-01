
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
                                  && (not . (isInfixOf $ pack ".png") . pack) fp
                                  && (not . (isInfixOf $ pack ".gif") . pack) fp) files
  items' <- sequence $ fmap (getIMG . ((</>) dir)) items
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






