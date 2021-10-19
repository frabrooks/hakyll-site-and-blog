
module BasicParsers where
--------------------------------------------------------------------
-- | Basic Parsers that are combined as building blocks to
--   create more interesting

import SiteParser

import Control.Applicative
import Data.Char
import System.FilePath
import System.Directory


-- | Parse a single character
charP :: Char -> Parser Char
charP x = Parser $ f
  where
    f (y:ys)
      | y == x = return (ys, x)
      | otherwise = failWith $ "Parse stopped! Expected: '" ++ (show x)
                             ++ "',but found: '" ++ (show y) ++ "'."
    f [] = failWith $ "Ran out of input looking for: " ++ (show x)


-- | Parse a single string
stringP :: String -> Parser String
stringP string = do
  logParse $ "Parsing string: " ++ (show string)
  sequenceA $ map charP string
 

-- | Consumes input until predicate false
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \ input ->
  let (token, rest) = span f input in
  if (token == "")
    then failWith "spanP pred false, aborting parse"
--    else MaybeT $ return $ Just (rest, token)
    else return (rest, token)


-- | Consumes input until predicate over two chars is false
spanP2 :: (Char -> Char -> Bool) -> Parser String
spanP2 f = Parser $ \ input -> aux "" input
  where
    aux :: String -> String ->  MaybeT (Writer [String]) (String , String)
    aux token ls@(x : y : z : xs)
      | f y z = aux (token ++ [ x ]) (tail ls)
      | token == "" = MaybeT $ return $ Nothing
      | otherwise = MaybeT $ return $ Just (tail ls, token ++ [ x ]) 
    aux "" rest = MaybeT $ return $ Nothing
    -- Not enough input/ ran out of input to parse spanP2
    aux token rest = MaybeT $ return $ Nothing


-- | Parse a line (NB: Will not parse empty line)
lineP :: Parser String 
lineP = dropTrailingSpaces <$> spanP (/= '\n')


-- | Whitespace parser (consume whitespace; including \n \r \t etc.)
ws :: Parser String
ws = spanP (isSpace)


-- | Space parser/consumer - \n & \r
sP :: Parser String
sP = spanP (\ c -> (c /= '\n' && c /= '\r') && (isSpace c))


-- | Parse a list of things separated by something
--   that will be parsed by the first parser
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> (many (sep *> element))
                    <|> (do
                            logParse "List parse finished"
                            return [])


-- | Parse something that may or may not be encapsulated in whitespace
inWS :: Parser a -> Parser a
inWS p = (many ws) *> p <* (many ws)


-- | Parse something that may or may not be encapsulated in non-breaking whitespace
inSP :: Parser a -> Parser a
inSP p = (many sP) *> p <* (many sP)


-- | Read one char of input
readCharP :: Parser Char
readCharP = Parser $ f
  where
    f (y:ys) = return (ys, y)
    f [] = failWith $ "Ran out of input but was told to parse next char!"


-- | Read until a String is encountered
untilStringP :: String -> Parser String
untilStringP string = Parser $ f
  where
    f [] = failWith $ "Ran out of input while looking for " ++ string ++ " in untilStringP! "
    f xs = if length xs < length string
           then failWith $ "Ran out of input while looking for " ++ string ++ " in untilStringP! "
           else
             let string' = take (length string) xs in
               if string' == string
               then return (drop (length string) xs, "")
               else do
                 (rest,tok) <- f (tail xs)
                 return (rest, (head xs) : tok)


--------------------------------------------------------------------
-- Helper functions

dropTrailingSpaces :: String -> String
dropTrailingSpaces = reverse . (dropWhile (isSpace)) . reverse

writeLog :: String -> Writer [String] ()
writeLog msg = Writer ((), [ msg ])

failWith :: String -> MaybeT (Writer [String]) a
failWith log = MaybeT $ do
  writeLog log
  return Nothing

logParse :: String -> Parser (a -> a)
logParse string = Parser $ \ input -> do
  lift (writeLog string)
  return (input, id)

nonConsume :: Parser a -> Parser a
nonConsume p = Parser $ \ input -> do
  (_, a) <- runParser p input
  return (input, a)

-- | Succeed on fail (returning input unchanged) and fail on success
notP :: Parser a -> Parser String
notP p = Parser $ \ input ->
  case fst . runWriter . runMaybeT $ runParser p input of
    Nothing -> MaybeT $ return $ Just (input, "")
    otherwise -> MaybeT $ return $ Nothing

    
jpgORpng :: String -> IO String
jpgORpng path = do
  b <- doesFileExist (path -<.> ".jpg")
  case b of
    True -> return $ path -<.> ".jpg"
    False -> return $ path -<.> ".png"



