{-# LANGUAGE DeriveGeneric #-}

module SiteData where

import GHC.Generics

import Data.Foldable (asum)
import Data.Maybe (fromJust)
import Data.Time

data ITEM_TYPE = Post | Merge | Pull deriving (Show, Eq)

data OpenSrcItem = OpenSrcItem { srcType  :: ITEM_TYPE
                               , srcTitle :: String
                               , srcDate :: String
                               , linesAdded :: String
                               , linesRemoved :: String
                               , srcLink  :: String
                               , srcDescription :: [String]
                               , srcTags :: [String]
                               } deriving (Show)

data OpenSrcJrnl = OpenSrcJrnl { jrnlIntro :: [String]
                               , jrnlItems :: [OpenSrcItem]
                               } deriving (Show)


data BlogPostMetaData = BlogPostMetaData { postTitle :: String
                                         , postDate :: String
                                         , postLink  :: String
                                         , isTechnical :: Bool 
                                         , postDesc :: String
                                         , thumbnail :: Maybe String
                                         , postTags :: [String]
                                         } deriving (Show, Generic)

data AboutMe = AboutMe { firstName :: String
                       , lastName :: String
                       , subtitleLHS :: String
                       , subtitleRHS :: String
                       , email :: String
                       , aboutMePars :: [String]
                       } deriving (Show)


data PortfolioItem = PortfolioItem { itemTitle :: String
                                   , itemLink :: String
                                   , itemDesc :: [String]
                                   , itemTags :: [String]
                                   , itemImgSrc :: String
                                   , gitHubLink :: Maybe String
                                   , pdfLink :: Maybe String
                                   } deriving (Show)

type AcademicWorkList = [PortfolioItem]

type ProjectPortfolio = [PortfolioItem]


data ActivityItem = ActivityItem { itemType :: ITEM_TYPE
                                 , activityDate :: Day
                                 , activityTitle :: String
                                 , linkToItem :: String
                                 } deriving (Show, Eq)

-- Ordered by Date
instance Ord ActivityItem where
  compare a b = compare (activityDate a) (activityDate b)
  (<) a b = (<) (activityDate a) (activityDate b)
  (<=) a b = (<=) (activityDate a) (activityDate b)
  (>) a b = (>) (activityDate a) (activityDate b)
  (>=) a b = (>=) (activityDate a) (activityDate b)
  max a b = case compare (activityDate a) (activityDate b) of
              LT -> b
              GT -> a
              EQ -> a
  min a b = case compare (activityDate a) (activityDate b) of
              LT -> a
              GT -> b
              EQ -> a


combineAsActivities :: [OpenSrcItem] -> [BlogPostMetaData] -> [ActivityItem]
combineAsActivities srcs posts = (fmap f posts) ++ (fmap g srcs)
  where
    f (BlogPostMetaData title date link _ _ _ _) = ActivityItem Post (p date) title link
    g (OpenSrcItem iType title date _ _ link _ _) = ActivityItem iType (p date) title link
    p = fromJust . tryParseTime


-- | Try and parse the time from a String outputting as Data.Time.Day
--   Fail and crash execution if no time can be parsed as this means bad input
--   that should be fixed before site goes live.
--
--   NB that, stupidly, %Y will parse both four and two char years but %y will
--   only parse two char years. %Y 96 gives 0096, whereas %y gives 1996. So the
--   %y's need to come first in the asum below else we'll be off by 2000 years
tryParseTime :: String -> Maybe Day
tryParseTime dateString = asum [
  parseTimeM True defaultTimeLocale "%d/%m/%y" dateString , -- try format: 02/08/96
  parseTimeM True defaultTimeLocale "%d-%m-%y" dateString , -- try format: 02-08-96      
  parseTimeM True defaultTimeLocale "%d/%m/%Y" dateString , -- try format: 02/08/1996
  parseTimeM True defaultTimeLocale "%d-%m-%Y" dateString , -- try format: 02-08-1996
  parseTimeM True defaultTimeLocale "%F" dateString ] -- try format: 1996-08-02



getTimeFormatter :: (FormatTime t) => t -> String
getTimeFormatter = (\ t ->
                      let day = formatTime defaultTimeLocale "%e" t in
                      let day' = case (head day) of ' ' -> tail day ; _ -> day in
                      day' ++ (dayPostfix day') ++ " of " ++ (formatTime defaultTimeLocale "%b" t)
                   )


dayPostfix :: String -> String
dayPostfix ('1' : []) = "st"
dayPostfix ('2' : []) = "nd"
dayPostfix ('3' : []) = "rd"
dayPostfix ('4' : []) = "th"
dayPostfix ('5' : []) = "th"
dayPostfix ('6' : []) = "th"
dayPostfix ('7' : []) = "th"
dayPostfix ('8' : []) = "th"
dayPostfix ('9' : []) = "th"
dayPostfix ('1' : '0' : []) = "th"
dayPostfix ('1' : '1' : []) = "th"
dayPostfix ('1' : '2' : []) = "th"
dayPostfix ('1' : '3' : []) = "th"
dayPostfix ('1' : '4' : []) = "th"
dayPostfix ('1' : '5' : []) = "th"
dayPostfix ('1' : '6' : []) = "th"
dayPostfix ('1' : '7' : []) = "th"
dayPostfix ('1' : '8' : []) = "th"
dayPostfix ('1' : '9' : []) = "th"
dayPostfix ('2' : '0' : []) = "th"
dayPostfix ('2' : '1' : []) = "st"
dayPostfix ('2' : '2' : []) = "sn"
dayPostfix ('2' : '3' : []) = "rd"
dayPostfix ('2' : '4' : []) = "th"
dayPostfix ('2' : '5' : []) = "th"
dayPostfix ('2' : '6' : []) = "th"
dayPostfix ('2' : '7' : []) = "th"
dayPostfix ('2' : '8' : []) = "th"
dayPostfix ('2' : '9' : []) = "th"
dayPostfix ('3' : '0' : []) = "th"
dayPostfix ('3' : '1' : []) = "st"


