{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | An overview of all the IRC gubbins, stats, charts, pretty pictures, etc.

module Ircbrowse.View.Overview
  (overview)
  where

import Ircbrowse.View
import Ircbrowse.View.Template
import Ircbrowse.View.Cloud

overview :: Maybe String -> Maybe String -> Range -> Stats -> Html
overview _ _ range stats = do
  template "overview" cloudScripts $ do
    container $ do
      row $ do
        span12 $ summarize range stats
      row $ do
        span6 $ mostActiveTimes stats
        span6 $ dailyActivity range stats
      row $ do
        span6 $ activeNicks stats
        span6 $ nickCloud (stActiveNicks stats)

summarize :: Range -> Stats -> Html
summarize range stats = p $ do
  h1 $ "IRC Browse"
  p $ do strong "Network(s): "
         toHtml (intercalate ", " (map snd (stNetworks stats)))
  p $ do strong "Channel(s): "
         forM_ (stChannels stats) $ \(network,chan) ->
           a ! href (toValue ("/browse/" ++ network ++ "/" ++ chan)) $ do "#"; toHtml chan
  "During this "
  toHtml (show (diffDays (rangeTo range) (rangeFrom range)))
  "-day reporting period, a total of "
  em $ toHtml $ showCount (stNickCount stats)
  " different nicks spoke. "
  "For a total "
  em $ toHtml $ showCount (stEventCount stats)
  " IRC events, there were "
  em $ toHtml $ showCount (stMsgCount stats)
  " messages of conversation."

mostActiveTimes :: Stats -> Html
mostActiveTimes stats = do
  h2 "Most Active Times"
  p "The most active times of the day in which anyone spoke."
  p $ img ! src (toValue url)

  where url = "http://chart.apis.google.com/chart?chxl=0:|" ++
              intercalate "|" labels ++
              "&chxt=x,y&chd=t:" ++
              intercalate "," datas ++
              "&chs=" ++ show w ++ "x" ++ show h ++ "&cht=bvs&chbh=a"
        labels = map show [0..23]
        datas = map (\x -> show (round ((fi x / maxcount) * 61))) times
        times = map snd (stActiveTimes stats)
        maxcount = fi (maximum times)
        w = 450
        h = 200

dailyActivity :: Range -> Stats -> Html
dailyActivity range stats = do
  h2 $ do "Daily Activity"
          " (last "
          toHtml (show (diffDays (rangeTo range) (rangeFrom range)))
          " days)"
  p "The amount of conversation per day in the past month."
  p $ img ! src (toValue url)

  where url = "http://chart.apis.google.com/chart?chxl=0:|" ++
              intercalate "|" labels ++
              "&chxt=x,y&chd=t:" ++
              intercalate "," datas ++
              "&chs=" ++ show w ++ "x" ++ show h ++ "&cht=bvs&chbh=a"
        labels = map (show.fst) (stDailyActivity stats)
        datas = map (\x -> show (round ((fi x / maxcount) * 61))) times
        times = map snd (stDailyActivity stats)
        maxcount = fi (maximum times)
        w = 450
        h = 200

activeNicks :: Stats -> Html
activeNicks stats = do
  h2 $ do "Most Active Nicks (Top "; toHtml (show limit); ")"
  table !. "table" $ do
    thead $ mapM_ th ["","Nick","Lines"]
    tbody $
      forM_ (zip [1..] (take limit (stActiveNicks stats))) $ \(i,(nick,linecount)) ->
        tr $ do
          td $ toHtml (show i)
          td $ toHtml nick
          td $ toHtml (showCount linecount)

  where limit = 10

nickCloud :: [(String,Integer)] -> Html
nickCloud stats = do
  h2 "Nicks Word Cloud"
  cloud "overview-nicks-container" (400,400) 100 3 stats
  p $ a ! href "/nick-cloud" $ "See full nick cloud →"
