-- | General statistics.

module Ircbrowse.Model.Stats where

import Ircbrowse.Types
import Ircbrowse.Data
import Ircbrowse.Types.Import

import Numeric
import Snap.App
import System.Random

getStats :: Channel -> Range -> Model c s Stats
getStats channel range@(Range from to) = do
  count <- single ["SELECT COUNT(*)"
                  ,"FROM event"
                  ,"WHERE channel = ? and timestamp > ? and timestamp < ?"]
                  (cid,from,to)
  msgcount <- single ["SELECT COUNT(*)"
                     ,"FROM event"
                     ,"WHERE type = 'talk'"
                     ,"AND channel = ? and timestamp > ? and timestamp < ?"]
                     (cid,from,to)
  nicks <- single ["SELECT COUNT(DISTINCT nick)"
                  ,"FROM event"
                  ,"WHERE channel = ? and type = 'talk'"
                  ,"AND timestamp > ? and timestamp < ?"]
                  (cid,from,to)
  activetimes <- query ["SELECT DATE_PART('HOUR',timestamp)::int,COUNT(*)"
                       ,"FROM EVENT"
                       ,"WHERE channel = ? and type = 'talk'"
                       ,"AND timestamp > ? AND timestamp < ?"
                       ,"GROUP BY DATE_PART('HOUR',timestamp)"
                       ,"ORDER BY 1 ASC"]
                       (cid,from,to)
  dailyactivity <- query ["SELECT date_part('day',date)::int,count FROM"
                         ," (SELECT timestamp::date as date,COUNT(*) as count"
                         ,"  FROM EVENT"
                         ,"  WHERE channel = ? and type = 'talk'"
                         ,"  AND timestamp > ? AND timestamp < ?"
                         ,"  GROUP BY timestamp::date"
                         ,"  ORDER BY 1 ASC) c"]
                         (cid,from,to)
  nickstats <- getNickStats channel range
  networks <- queryNoParams ["SELECT name,title FROM network order by title"]
  channels <- queryNoParams ["SELECT network,name FROM channel order by name"]
  activitybyyear <- query ["SELECT year,lines FROM general_activity_by_year where channel = ? order by year asc"]
                          (Only cid)
  conversationbyyear <- query ["SELECT year,lines FROM conversation_by_year where channel = ? order by year asc"]
                              (Only cid)

  return Stats
    { stEventCount    = fromMaybe 0 count
    , stMsgCount      = fromMaybe 0 msgcount
    , stNickCount     = fromMaybe 0 nicks
    , stActiveTimes   = activetimes
    , stDailyActivity = dailyactivity
    , stActiveNicks   = nickstats
    , stNetworks      = networks
    , stChannels      = channels
    , stActivityByYear = activitybyyear
    , stConversationByYear = conversationbyyear
   }

  where cid = showChanInt channel

getNickStats :: Channel -> Range -> Model c s [(String,Integer)]
getNickStats channel (Range from to) =
  query ["SELECT nick,COUNT(*)"
        ,"FROM EVENT"
        ,"WHERE channel = ? and type = 'talk'"
        ,"AND timestamp > ?"
        ,"AND timestamp < ?"
        ,"GROUP BY nick"
        ,"ORDER BY 2 DESC"
        ,"LIMIT 200"]
        (showChanInt channel,from,to)

-- | Some test stats.
sampleStats :: Stats
sampleStats         = Stats
  { stEventCount    = 110000
  , stMsgCount      = 100000
  , stNickCount     = 1000
  , stActiveNicks   = sortBy (flip (comparing snd))
                             (zipWith (\r i -> (showHex (r+i) "",r))
                                      (randomRs (0,10000) (mkStdGen 1))
                                      [1..10])
  , stActiveTimes   = zipWith (\r hour -> (hour,r))
                              (randomRs (0,60) (mkStdGen 2))
                              [0..23]
  , stDailyActivity = zipWith (\r day -> (day,r))
                              (randomRs (0,60) (mkStdGen 3))
                              [1..31]
  , stNetworks      = [("freenode","Freenode")]
  , stChannels      = [("freenode","haskell")]
  , stActivityByYear = []
  , stConversationByYear = []
  }
