-- | Downloading logs from tunes.org.

module Ircbrowse.Tunes where

import Data.ByteString as L
import Data.Maybe
import Data.Time
import Network.Curl
import System.Locale
import System.FilePath
import System.Directory

-- | Possible supported channels.
data Channel = Haskell | Lisp
  deriving Enum

-- | Download the log for a channel on a given day.
downloadLog :: Channel -> Day -> IO (Either (CurlCode,ByteString) FilePath)
downloadLog channel day = do
  tmp <- getTemporaryDirectory
  let cachedfp = tmp </> unmakeDay day
  exists <- doesFileExist cachedfp
  if exists
     then do Prelude.putStrLn $ "Importing from cached " ++ cachedfp
     	     return (Right cachedfp)
     else withCurlDo $ do
	    let dl = (makeUrl channel day)
	    Prelude.putStrLn $ "Downloading " ++ dl
	    (code,resp) <- curlGetString_ dl []
	    case code of
	      CurlOK -> do L.writeFile cachedfp resp
	      	     	   return (Right cachedfp)
	      _ -> return (Left (code,resp))

-- Make a day, useful for REPL.
makeDay :: String -> Day
makeDay = fromJust . parseDay

-- | Parse a day from a string.
parseDay :: String -> Maybe Day
parseDay = parseTime defaultTimeLocale "%Y-%m-%d"

-- | Parse a day from a string.
parseTunesDay :: String -> Maybe Day
parseTunesDay = parseTime defaultTimeLocale "%y.%m.%d"

-- | Make a URL for tunes.org.
makeUrl :: FormatTime t => Channel -> t -> URLString
makeUrl channel t =
  "http://tunes.org/~nef/logs/" ++ showChan channel ++
  "/" ++ unmakeDay t

unmakeDay :: FormatTime t => t -> String
unmakeDay = formatTime defaultTimeLocale "%y.%m.%d"

-- | Show a channel.
showChan :: Channel -> String
showChan Haskell = "haskell"
showChan Lisp = "lisp"

-- | Show a channel.
showChanInt :: Channel -> Int
showChanInt Haskell = 1
showChanInt Lisp = 2

-- | Read a channel.
parseChan :: String -> Maybe Channel
parseChan "haskell" =  Just Haskell
parseChan "lisp" =  Just Lisp
parseChan _ = Nothing

idxNum :: Channel -> Int
idxNum Haskell = 1000
idxNum Lisp = 2000
