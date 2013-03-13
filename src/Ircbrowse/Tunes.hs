-- | Downloading logs from tunes.org.

module Ircbrowse.Tunes where

import Data.ByteString
import Data.Maybe
import Data.Time
import Network.Curl
import System.Locale

-- | Possible supported channels.
data Channel = Haskell | Lisp
  deriving Enum

-- | Download the log for a channel on a given day.
downloadLog :: Channel -> Day -> IO (Either (CurlCode,ByteString) ByteString)
downloadLog channel day = do
  withCurlDo $ do
    let dl = (makeUrl channel day)
    Prelude.putStrLn $ "Downloading " ++ dl
    (code,resp) <- curlGetString_ dl []
    case code of
      CurlOK -> return (Right resp)
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
-- showChan OCaml = "ocaml"

-- | Show a channel.
showChanInt :: Channel -> Int
showChanInt Haskell = 1
showChanInt Lisp = 2
-- showChanInt OCaml = 3

-- | Read a channel.
parseChan :: String -> Maybe Channel
parseChan "haskell" =  Just Haskell
parseChan "lisp" =  Just Lisp
-- parseChan "ocaml" =  Just OCaml
parseChan _ = Nothing
