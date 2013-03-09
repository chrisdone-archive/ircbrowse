module Ircbrowse.Model.Stats where

import Ircbrowse.Types
import Ircbrowse.Data

import Numeric
import Snap.App
import System.Random

getStats :: Maybe String -> Maybe String -> Range -> Model c s Stats
getStats network channel (Range from to) = do
  count <- single ["SELECT COUNT(*)"
                  ,"FROM event"
                  ,"WHERE timestamp > ? and timestamp < ?"]
                  (from,to)
  msgcount <- single ["SELECT COUNT(*)"
                     ,"FROM event"
                     ,"WHERE type = 'talk'"
                     ,"AND timestamp > ? and timestamp < ?"]
                     (from,to)
  nicks <- single ["SELECT COUNT(DISTINCT nick)"
                  ,"FROM event"
                  ,"WHERE type = 'talk'"
                  ,"AND timestamp > ? and timestamp < ?"]
                  (from,to)
  activetimes <- query ["SELECT DATE_PART('HOUR',timestamp)::int,COUNT(*)"
                       ,"FROM EVENT"
                       ,"WHERE type = 'talk'"
                       ,"AND timestamp > ? AND timestamp < ?"
                       ,"GROUP BY DATE_PART('HOUR',timestamp)"
                       ,"ORDER BY 1 ASC"]
                       (from,to)
  dailyactivity <- query ["SELECT date_part('day',date)::int,count FROM"
                         ," (SELECT timestamp::date as date,COUNT(*) as count"
                         ,"  FROM EVENT"
                         ,"  WHERE type = 'talk'"
                         ,"  AND timestamp > ? AND timestamp < ?"
                         ,"  GROUP BY timestamp::date"
                         ,"  ORDER BY 1 ASC) c"]
                         (from,to)
  activenicks <- query ["SELECT nick,COUNT(*)"
                       ,"FROM EVENT"
                       ,"WHERE type = 'talk'"
                       ,"AND timestamp > ?"
                       ,"AND timestamp < ?"
                       ,"GROUP BY nick"
                       ,"ORDER BY 2 DESC"
                       ,"LIMIT 50"]
                       (from,to)
  networks <- queryNoParams ["SELECT name,title FROM network order by title"]
  channels <- queryNoParams ["SELECT network,name FROM channel order by name"]
  return Stats
    { stEventCount    = fromMaybe 0 count
    , stMsgCount      = fromMaybe 0 msgcount
    , stNickCount     = fromMaybe 0 nicks
    , stActiveTimes   = activetimes
    , stDailyActivity = dailyactivity
    , stActiveNicks   = activenicks
    , stNetworks      = networks
    , stChannels      = channels
   }

-- | Some test stats.
sampleStats :: Stats
sampleStats         = Stats
  { stEventCount    = 110000
  , stMsgCount      = 100000
  , stNickCount     = 1000
  , stActiveNicks   = sortBy (flip (comparing snd))
                             (zipWith (\r i -> (showHex (r+i) "",r))
                                      (randomRs (0,10000) (mkStdGen 1))
                                      [0..10])
  , stActiveTimes   = zipWith (\r hour -> (hour,r))
                              (randomRs (0,60) (mkStdGen 2))
                              [0..23]
  , stDailyActivity = zipWith (\r day -> (day,r))
                              (randomRs (0,60) (mkStdGen 3))
                              [1..31]
  , stNetworks      = [("freenode","Freenode")]
  , stChannels      = [("freenode","haskell")]
  }
