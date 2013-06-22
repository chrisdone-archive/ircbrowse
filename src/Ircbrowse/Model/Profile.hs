-- | Statistics for a specific nick's profiling.

module Ircbrowse.Model.Profile where

import Ircbrowse.Types
import Ircbrowse.Data
import Ircbrowse.Tunes

import Snap.App
import Data.Text (Text)

data NickStats = NickStats
  { nickHours :: [(Int,Int)]
  , nickLines :: Int
  , nickYears :: [Int]
  }

activeHours :: Text -> Model c s NickStats
activeHours nick = do
  hours <- query ["SELECT"
                 ,"DATE_PART('HOUR',timestamp) :: integer AS hour,"
                 ,"COUNT(*) AS lines"
                 ,"FROM event"
                 ,"WHERE nick = ? and type = 'talk'"
                 ,"GROUP BY DATE_PART('HOUR',timestamp)"
                 ,"ORDER BY hour;"]
                 (Only nick)
  lines <- single ["SELECT COUNT(*) FROM event WHERE nick = ? AND type = 'talk'"]
                  (Only nick)
  years <- query ["SELECT DATE_PART('YEAR',timestamp) :: integer"
                 ,"FROM event"
                 ,"WHERE nick = ? and type = 'talk'"
                 ,"GROUP BY DATE_PART('YEAR',timestamp)"]
                 (Only nick)
  return $
    NickStats { nickHours = hours
              , nickLines = fromMaybe 0 lines
              , nickYears = map (\(Only x) -> x) years
              }
