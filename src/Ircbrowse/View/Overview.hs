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
import Ircbrowse.View.Chart
import Ircbrowse.Tunes

import Control.Arrow

overview :: Maybe Channel -> Range -> Stats -> Html
overview _ range stats = do
  template "overview" "IRC Browse" cloudScripts $ do
    container $ do
      row $ do
        span12 $ summarize range stats
      row $ do
        span6 $ mostActiveTimes stats
        span6 $ dailyActivity range stats
      row $ do
        span6 $ activeNicks stats
        span6 $ nickCloud (stActiveNicks stats)
      row $ do
        span6 $ activityByYear stats
        span6 $ conversationByYear stats
    footer

summarize :: Range -> Stats -> Html
summarize range stats = p $ do
  h1 $ "IRC Browse"
  p $ do strong "Channel(s): "
         htmlCommasAnd $ flip map (stChannels stats) $ \(network,chan) ->
           a ! href (toValue ("/browse/" ++ chan)) $ do "#"; toHtml chan
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
  barChart (map (first show) (stActiveTimes stats))

dailyActivity :: Range -> Stats -> Html
dailyActivity range stats = do
  h2 $ do "Daily Activity"
          " (last "
          toHtml (show (diffDays (rangeTo range) (rangeFrom range)))
          " days)"
  p "The amount of conversation per day in the past month."
  barChart (map (first show) (stDailyActivity stats))

activeNicks :: Stats -> Html
activeNicks stats = do
  h2 $ do "Most Active Nicks (Top "; toHtml (show limit); ")"
  table !. "table" $ do
    thead $ mapM_ th ["","Nick","Lines"]
    tbody $
      forM_ (zip [1..] (take limit (stActiveNicks stats))) $ \(i,(nick,linecount)) ->
        tr $ do
          td $ toHtml (show i)
          td $ a ! href (toValue ("/nick/" ++ nick)) $ toHtml nick
          td $ toHtml (showCount linecount)
  p $ a ! href "/social" $ "See social graph →"

  where limit = 10

nickCloud :: [(String,Integer)] -> Html
nickCloud stats = do
  h2 "Nicks Word Cloud"
  cloud "overview-nicks-container" (400,400) 100 3 stats
  p $ a ! href "/nick-cloud" $ "See full nick cloud →"

activityByYear :: Stats -> Html
activityByYear stats = do
  h2 "Activity by Year"
  p "Number of lines of general activity per year."
  barChart (map (first show) (stActivityByYear stats))

conversationByYear :: Stats -> Html
conversationByYear stats = do
  h2 "Conversation by Year"
  p "Number of lines of conversation year."
  barChart (map (first show) (stConversationByYear stats))
