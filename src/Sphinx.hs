{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Use the command-line program for Sphinx to search.

module Sphinx where

import           Control.Applicative
import           Control.Monad.IO
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Default
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
  , sLimit  :: !Int
  , sOffset :: !Int
  } deriving Show

instance Default Sphinx where
  def = Sphinx "sphinx" "" "./sphinx.conf" 35 0

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
    runInteractiveProcess (sPath sphinx) args n n
  exitCode <- waitForProcess ph
  case exitCode of
    ExitSuccess -> do output <- S.hGetContents pout
                      case parse output of
                        Nothing -> return (Left "Unable to parse Sphinx search output.")
                        Just out -> return (Right out)
    ExitFailure _ -> do err <- hGetContents perr
                        return (Left err)

  where n = Nothing
        args = ["--config",sConfig sphinx,T.unpack (sQuery sphinx)
               ,"--limit",show (sLimit sphinx)
               ,"--offset",show (sOffset sphinx)
               ,"--sort=date"]

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
    results <- parseResults (S.drop (S.length consumed) input)
    let !count = length results
    Result <$> pure (decodeUtf8 index)
           <*> pure (decodeUtf8 query)
           <*> pure count
           <*> readInt (decodeUtf8 total)
           <*> readDouble (decodeUtf8 timing)
           <*> pure results
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

readInt x = case T.decimal x of
              Right (ok,"") -> Just ok
              _ -> Nothing
readDouble x = case T.double x of
                 Right (ok,"") -> Just ok
                 _ -> Nothing
