{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | An overview of all the IRC gubbins, stats, charts, pretty pictures, etc.

module Ircbrowse.View.Overview
  (overview,stats)
  where

import Ircbrowse.Types.Import
import Ircbrowse.View
import Ircbrowse.View.Chart
import Ircbrowse.View.Cloud
import Ircbrowse.View.Template
import Ircbrowse.View.Calendar

import Control.Arrow
import Data.Text (pack)

overview :: Html
overview =
  template "overview" "IRC Browse" cloudScripts $ do
    container $ do
      row $
        span12 $ do
          h1 "IRC Browse"
          p "Choose the channel:"
          forM_ [toEnum 0 ..] $ \(chan) ->
            h2 $
              a ! href (toValue (showChan chan)) $
                do "#"; toHtml (showChan chan)
    footer

stats :: Channel -> Range -> Stats -> Html
stats channel range stats = do
  template "overview" ("IRC Browse: #" <> pack (showChan channel)) cloudScripts $ do
    channelNav channel
    container $ do
      row $ do
        span12 $ do
          summarize channel range stats
      row $ do
        span6 $ mostActiveTimes stats
        span6 $ dailyActivity range stats
      row $ do
        span6 $ activeNicks channel stats
        span6 $ nickCloud channel (stActiveNicks stats)
      row $ do
        span6 $ activityByYear stats
        span6 $ conversationByYear stats
    footer

summarize :: Channel -> Range -> Stats -> Html
summarize channel range stats = p $ do
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
  h2 "Most Active Times (UTC)"
  p "The most active times of the day in which anyone spoke."
  radarChart (map (first show) (stActiveTimes stats))

dailyActivity :: Range -> Stats -> Html
dailyActivity range stats = do
  h2 $ do "Daily Activity"
          " (last "
          toHtml (show (diffDays (rangeTo range) (rangeFrom range)))
          " days)"
  p "The amount of conversation per day in the past month."
  barChart (map (first show) (stDailyActivity stats))

activeNicks :: Channel -> Stats -> Html
activeNicks channel stats = do
  h2 $ do "Most Active Nicks (Top "; toHtml (show limit); ")"
  table !. "table" $ do
    thead $ mapM_ th ["","Nick","Lines"]
    tbody $
      forM_ (zip [1..] (take limit (stActiveNicks stats))) $ \(i,(nick,linecount)) ->
        tr $ do
          td $ toHtml (show i)
          td $ a ! href (toValue ("/nick/" ++ nick)) $ toHtml nick
          td $ toHtml (showCount linecount)
  p $ a ! href (toValue ("/nicks/" ++ showChan channel)) $ "See all nicks →"

  where limit = 10

nickCloud :: Channel -> [(String,Integer)] -> Html
nickCloud channel stats = do
  h2 "Nicks Word Cloud"
  cloud "overview-nicks-container" (400,400) 100 3 stats
  p $ a ! href (toValue ("/nick-cloud/" ++ showChan channel)) $ "See full nick cloud →"

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
