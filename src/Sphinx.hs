{-# LANGUAGE OverloadedStrings #-}

-- | Use the command-line program for Sphinx to search.

module Sphinx where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Default
import           Data.Text
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.Text.Read as T
import           System.Exit
import           System.IO
import           System.Process
import           Text.Regex.PCRE.Light

data Sphinx = Sphinx
  { sPath   :: !FilePath
  , sQuery  :: !Text
  , sConfig :: !FilePath
  } deriving Show

instance Default Sphinx where
  def = Sphinx "sphinx" "" "./sphinx.conf"

data Result = Result
  { rIndex    :: !Text
  , rQuery    :: !Text
  , rReturned :: !Int
  , rTotal    :: !Int
  , rSecs     :: !Double
  , rResults  :: ![(Int,Int)]
  } deriving Show

-- | Search with the given query.
search :: Sphinx -> IO (Either String Result)
search sphinx = do
  (_,pout,perr,ph) <-
    runInteractiveProcess (sPath sphinx) ["--config",sConfig sphinx,T.unpack (sQuery sphinx)] n n
  exitCode <- waitForProcess ph
  case exitCode of
    ExitSuccess -> do output <- S.hGetContents pout
                      case parse output of
                        Nothing -> return (Left "Unable to parse Sphinx search output.")
                        Just out -> return (Right out)
    ExitFailure _ -> do err <- hGetContents perr
                        return (Left err)

  where n = Nothing

-- | Escape the text.
escapeText :: Text -> Text
escapeText = T.intercalate "\\" . breakBy (`elem` escapedChars)
    where breakBy p t | T.null t  = [T.empty]
                      | otherwise = (if p $ T.head t then ("":) else id) $ T.groupBy (\_ x -> not $ p x) t
          escapedChars =  '"':'\\':"-!@~/()*[]="

-- | Parse the results header.
parse :: ByteString -> Maybe Result
parse input = case match rx input [] of
  Just [consumed,index,query,returned,total,timing] -> do
    Result <$> pure (decodeUtf8 index)
           <*> pure (decodeUtf8 query)
           <*> readInt (decodeUtf8 returned)
           <*> readInt (decodeUtf8 total)
           <*> readDouble (decodeUtf8 timing)
           <*> parseResults (S.drop (S.length consumed) input)
  _ -> Nothing

  where rx = compile "index '(.+)': query '(.+) ': \
                     \returned ([0-9]+) matches of ([0-9]+) \
                     \total in ([0-9.]+) sec\n\
                     \\n\
                     \displaying matches:\n"
                     []


-- | Parse the results list.
parseResults :: ByteString -> Maybe [(Int,Int)]
parseResults input = case match rx input [] of
  Just [consumed,documentId,weight] -> do
    x <- (,) <$> readInt (decodeUtf8 documentId)
             <*> readInt (decodeUtf8 weight)
    xs <- parseResults (S.drop (S.length consumed) input) <|> return []
    return (x : xs)
  _ -> Nothing
  where rx = compile "\n[0-9]+\\. document=([0-9]+), weight=([0-9]+)[^\n]+"
                     []

-- Sphinx 2.0.5-release (r3308)
-- Copyright (c) 2001-2012, Andrew Aksyonoff
-- Copyright (c) 2008-2012, Sphinx Technologies Inc (http://sphinxsearch.com)

-- using config file '../sphinx.conf'...
-- index 'event_texts': query 'potato ': returned 14 matches of 14 total in 0.000 sec

-- displaying matches:
-- 1. document=252015, weight=1755, timestamp=Mon Feb 11 08:09:47 2013, network=0, channel=0, nick=flebron
-- 2. document=1194611, weight=1755, timestamp=Wed May 30 12:44:11 2012, network=0, channel=0, nick=mcstar
-- 3. document=1497120, weight=1755, timestamp=Mon Jul 23 10:24:26 2012, network=0, channel=0, nick=srhb
-- 4. document=1637414, weight=1755, timestamp=Wed Aug 15 14:24:20 2012, network=0, channel=0, nick=merijn
-- 5. document=581587, weight=1685, timestamp=Tue Feb  7 12:02:49 2012, network=0, channel=0, nick=rostayob
-- 6. document=711295, weight=1685, timestamp=Sun Feb 26 16:15:47 2012, network=0, channel=0, nick=t7
-- 7. document=825929, weight=1685, timestamp=Sun Mar 18 22:44:00 2012, network=0, channel=0, nick=rostayob
-- 8. document=1404844, weight=1685, timestamp=Thu Jul  5 22:42:32 2012, network=0, channel=0, nick=mikeplus64
-- 9. document=1441364, weight=1685, timestamp=Thu Jul 12 16:07:49 2012, network=0, channel=0, nick=Nereid
-- 10. document=1543918, weight=1685, timestamp=Sun Jul 29 01:45:58 2012, network=0, channel=0, nick=mikeplus64
-- 11. document=1543936, weight=1685, timestamp=Sun Jul 29 01:46:19 2012, network=0, channel=0, nick=mikeplus64
-- 12. document=1543940, weight=1685, timestamp=Sun Jul 29 01:46:20 2012, network=0, channel=0, nick=lambdabot
-- 13. document=1543948, weight=1685, timestamp=Sun Jul 29 01:46:26 2012, network=0, channel=0, nick=mikeplus64
-- 14. document=1543959, weight=1685, timestamp=Sun Jul 29 01:46:32 2012, network=0, channel=0, nick=mikeplus64

-- words:
-- 1. 'potato': 14 documents, 18 hits

readInt x = case T.decimal x of
              Right (ok,"") -> Just ok
              _ -> Nothing
readDouble x = case T.double x of
                 Right (ok,"") -> Just ok
                 _ -> Nothing
