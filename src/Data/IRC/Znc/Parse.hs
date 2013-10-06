{-# LANGUAGE ViewPatterns #-}
module Data.IRC.Znc.Parse where

import           Data.IRC.Event

import           Control.Applicative
import           Data.List
import           Data.Maybe
import           Data.Word
import           System.Locale

import qualified Data.Foldable            as F
import qualified Data.Attoparsec          as P
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as B8
import qualified Data.Time                as Time
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified System.FilePath          as Path
import qualified System.Environment       as Env
import qualified System.IO.Error          as IOError
import qualified Data.Time.LocalTime.TimeZone.Series as Zone
import qualified Data.Time.LocalTime.TimeZone.Olson  as Zone

-- | Configuring the parser.
data Config = Config
  { timeZone :: String   -- ^ Timestamp time zone; an Olson time zone name.
  , zoneInfo :: FilePath -- ^ Directory for time zone files; @$TZDIR@ overrides.
  } deriving (Show)

-- | @'Config'@ value suitable for parsing @#haskell@ logs on Linux.
ircbrowseConfig :: Config
ircbrowseConfig = Config
  { timeZone = "Europe/Berlin"
  , zoneInfo = "/usr/share/zoneinfo" }

-- Many text encodings are used on IRC.
-- We decode clog metadata as ASCII.
-- We parse messages as UTF-8 in a lenient mode.

decode :: B.ByteString -> T.Text
decode = T.decodeUtf8With T.lenientDecode


-- Timestamps are in local time and must be converted.

type TimeConv = Time.LocalTime -> Time.UTCTime

getTimeConv :: FilePath -> IO TimeConv
getTimeConv p = Zone.localTimeToUTC' <$> Zone.getTimeZoneSeriesFromOlsonFile p

data TimeAdj = TimeAdj Time.Day TimeConv


-- Parsers.

notNewline :: Word8 -> Bool
notNewline w = w /= 13 && w /= 10

restOfLine :: P.Parser T.Text
restOfLine = decode <$> P.takeWhile notNewline <* P.take 1

nextLine :: P.Parser ()
nextLine = P.skipWhile notNewline <* P.take 1

digits :: Int -> P.Parser Int
digits n = atoi <$> P.count n digit where
  atoi = foldl' (\m d -> m*10 + fromIntegral d - 48) 0
  digit = P.satisfy isDigit
  isDigit w = w >= 48 && w <= 57

time :: TimeAdj -> P.Parser Time.UTCTime
time (TimeAdj day conv) = f <$> d2 <* col <*> d2 <* col <*> d2 where
  d2  = digits 2
  col = P.word8 58
  f h m s = conv . Time.LocalTime day $ Time.TimeOfDay h m (fromIntegral s)

event :: P.Parser Event
event = F.asum
  [ str " *** " *> F.asum
    [ userAct Join  "Joins: "
    , userAct Part  "Parts: "
    , userAct Quit  "Quits: "
    , ReNick <$> (nick <* str " is now known as ") <*> nick <* nextLine
    , Mode   <$> nick <*> (str " sets mode: " *> restOfLine)
    , (\kicked kicker x ->
        Kick kicked
             kicker
             (fromMaybe x (T.stripSuffix (T.pack ")") x))) <$>
      (nick <* str " was kicked by ") <*> (nick <* str " (") <*> restOfLine
    , (\x -> Topic (fromMaybe x (T.stripSuffix (T.pack "'") x))) <$>
          (nick *> str " changes topic to '" *> restOfLine)
    ]
  , Talk   <$ str " <"  <*> nick <*  str "> " <*> restOfLine
  , Notice <$ str " -"  <*> nick <*> restOfLine -- FIXME: parse host
  , Act    <$ str " * " <*> nick <*  chr ' '  <*> restOfLine
  ] where
    chr  = P.word8  . fromIntegral . fromEnum
    nick = (Nick . decode) <$> P.takeWhile (not . P.inClass " \n\r\t\v<>")
    userAct f x = f <$ str x <*> nick <* chr ' ' <*> restOfLine
    global f x = f <$ str x <*> restOfLine

str  = P.string . B8.pack

line :: TimeAdj -> P.Parser EventAt
line adj =
  P.try (EventAt <$> (str "[" *> time adj <* str "]") <*> event)
  <|>   (NoParse <$> restOfLine)

safeRead :: (Read a) => String -> Maybe a
safeRead x | [(v,"")] <- reads x = Just v
safeRead _ = Nothing

getDay fp = case Path.splitFileName fp of
  (_,(drop 1 . dropWhile (/='_')) -> date) ->
    case Time.parseTime defaultTimeLocale "%Y%m%d.log" date of
      Just day -> day
      Nothing -> error ("cannot parse date from filename: " ++ date)

-- | Parse a log file.
--
-- The file name (after any directory) is significant.
-- It is used to set the date for timestamps.
-- It should have the form @YY.MM.DD@, as do the files on
-- @tunes.org@.
parseLog :: Config -> FilePath -> IO [EventAt]
parseLog (Config{timeZone=tz, zoneInfo=zi}) p = do
  tzdir <- either (const zi) id <$> IOError.try (Env.getEnv "TZDIR")
  adj   <- TimeAdj (getDay p) <$> getTimeConv (Path.combine tzdir tz)
  b <- B.readFile p
  let go r@P.Fail{}    = error $ show r
      go (P.Partial g) = go $ g B.empty
      go (P.Done _  x) = x
  let es = go $ P.parse (P.manyTill (line adj) P.endOfInput) b
  return es
