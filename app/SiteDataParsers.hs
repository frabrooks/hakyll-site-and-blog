
module SiteDataParsers where
-- | Parsers for the plain text files that store the content that gets
--   displayed on the site.

import SiteParser
import BasicParsers
import SiteData

import Control.Applicative
import Data.Char
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Text (pack, isInfixOf)



iTEM_DELIM :: Char
iTEM_DELIM = '-'

itemDelimP :: Parser String -- at least 5 iTEM_DELIM's (-)
itemDelimP = concat <$> ((:) <$> (stringP (replicate 5 iTEM_DELIM))
                             <*> many (stringP [ iTEM_DELIM ] ) )
pAR_BEGIN :: String
pAR_BEGIN = "#{"

pAR_END :: String
pAR_END = "}#"


-- | AboutMe Parser
aboutMeP :: Parser AboutMe
aboutMeP = content
  where content =
          (AboutMe)
          <$> inWS lineP -- First Name
          <*> inWS lineP -- Last Name
          <*> inWS lineP -- subtitlelhs
          <*> inWS lineP -- subtitlerhs
          <*> inWS lineP -- email
          <*> inWS parsP -- about me / desc


-- | PortfolioItem Parser: For items displayed on the index page either
--   under 'academic-work' or 'project-portfolio'
portfolioItemP :: String -> Parser PortfolioItem
portfolioItemP imgSrc = (makeItem) <$> inWS lineP <*> inWS parsP <*> inWS parsP <*> inWS tagsP
    where
      makeItem title links desc tags = PortfolioItem title (head links) desc tags imgSrc gh pdf
        where
          gh = let mpgh = filter ((isInfixOf $ pack "https://github.com") . pack) links
               in case mpgh of
                    [] -> Nothing
                    [ link ] -> Just link
                    _ -> undefined -- Should not be more than one GitHub link                  
          pdf = let mpdf = filter ((isInfixOf $ pack ".pdf") . pack) links
                in case mpdf of
                     [] -> Nothing
                     [ link ] -> Just link
                     _ -> undefined -- Should not be more than one pdf link

---- | BlogPostList Parser
--blogPostListP :: Parser BlogPostList
--blogPostListP = content
--  where content =
--          (BlogPostList)
--          <$> inWS parsP
--          <*> (itemDelimP *>
--               sepBy (inWS $ itemDelimP) blogPostItemP)
--          where
--            blogPostItemP :: Parser BlogPostItem
--            blogPostItemP =
--              (BlogPostItem)
--              <$> inWS lineP -- Title
--              <*> inWS lineP -- Date
--              <*> inWS lineP -- link
--              <*> inWS lineP -- desc
--              <*> inWS tagsP


-- | OpenSrcJrnl Parser
openSrcJrnlP :: Parser OpenSrcJrnl
openSrcJrnlP = ws *> content
  where content =
          (OpenSrcJrnl)
          <$> inWS parsP
          <*> (itemDelimP *>
               sepBy (inWS $ itemDelimP) openSrcItemP)
          where
            openSrcItemP :: Parser OpenSrcItem
            openSrcItemP =
              (OpenSrcItem . toItemType)
              <$> inWS lineP -- Merge or Pull
              <*> inWS lineP -- Title
              <*> inWS lineP -- Date
              <*> inWS linesAddedP
              <*> inWS linesRemovedP
              <*> inWS lineP -- link
              <*> inWS parsP -- desc
              <*> inWS tagsP
            toItemType :: String -> ITEM_TYPE
            toItemType s
              | s == "Merge" = Merge
              | s == "Pull"  = Pull


firstBodyParP :: Parser String
firstBodyParP = untilStringP "id=\"body\"" *> untilStringP "<p>" *> untilStringP "</p>"


getMetaP :: Parser String
getMetaP = stringP "---" *> untilStringP "---" <* (many ws)


getDateP :: Parser String
getDateP = (reverse . tail . reverse) <$>
           (spanP (not . isDigit) *> spanP (\ c -> isDigit c || c == '-'))

getTitleP :: Parser String
getTitleP = untilStringP "title:" *> ws *> lineP


getDescP :: Parser String
getDescP = untilStringP "desc:" *> inWS lineP

getTagsP :: Parser [String]
getTagsP = untilStringP "tags:" *> inWS (sepBy (charP ',' *> ws) (spanP2 pred))
  where
    pred = (\ a b -> not (a == '\n' || a == ',') && (not (isSpace a) || (isAlpha b)))
    -- If a is a space, then b has to be an alphanumeric and a should
    -- never be newline or a comma

getCatP :: Parser Bool
getCatP = untilStringP "technical:" *> (return True) <|> return False


---------------------------------------------------------------
-- | Util Parsers 

parsBegin :: Parser String
parsBegin = stringP pAR_BEGIN

parsEnd :: Parser String
parsEnd = stringP pAR_END

parP :: Parser String
parP = notP parsEnd *> lineP

-- | Read paragraphs delineated by pAR_BEGIN and pAR_END
parsP :: Parser [String]
parsP =  do
  logParse "Parsing paragraphs"
  parsBegin *> inWS (sepBy ws parP) <* parsEnd


-- | Parse lines separated by whitespace before a newline
--   with a further newline delineating the end of the lines
tagsP :: Parser [String]
tagsP = stringP "@tags{" *>
        ( inWS $ (sepBy (inSP $ stringP "\n")  lineP) )
        <* stringP "}@tags"


linesAddedP :: Parser String
linesAddedP = do
  logParse $ "Parsing linesAddded (+XX)"
  charP '+' *> spanP isDigit


linesRemovedP :: Parser String
linesRemovedP = do
  logParse $ "Parsing linesRemoved (-XX)"
  charP '-' *> spanP isDigit






