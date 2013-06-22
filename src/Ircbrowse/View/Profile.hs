{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | Display a profile of the given nick.

module Ircbrowse.View.Profile
  (nickProfile)
  where

import Ircbrowse.View
import Ircbrowse.View.Template
import Ircbrowse.View.Cloud
import Ircbrowse.View.Chart
import Ircbrowse.Model.Profile
import Ircbrowse.Tunes

import qualified Text.Blaze.Html5 as H
import Data.Text (Text)
import Control.Arrow
import Text.Printf

nickProfile :: Text -> Bool -> NickStats -> Html
nickProfile nick showrecent NickStats{..} = do
  template "nick-profile" (H.title (toHtml nick)) $ do
    container $ do
      h1 (toHtml nick)
      tabNav $ do
        li !. (if showrecent then "active" else "") $ a ! href "?recent=true" $ "Recent"
        li !. (if not showrecent then "active" else "") $ a ! href "?recent=false" $ "All Time"
      row $
        span12 $
          if nickLines == 0
             then p $ do toHtml nick; " hasn't said anything."
             else do p $ do toHtml nick; " has written "
                            toHtml (showCount nickWords); " words "
                            toHtml (showCount nickLines); " lines"
                            " (averaging "; toHtml (show nickAvg); " words per line)"
                            ", "
                            " in "
                            if showrecent
                               then "the past 30 days, "
                               else do toHtml (show (length nickYears)) ; " year(s), "
                            "most active at around "
                            toHtml (printf "%.0d" (let (z,_,_,_) = (maximumBy' (comparing (\(_,_,_,z) -> z)) nickHours) in z) :: String)
                            ":00 (UTC)."


                     row $ do
                       span7 $ do
                         h2 "Active Hours (by line)"
                         p $ do
                           radarChart (map (\(h,_,_,lines) -> (show h,lines)) nickHours)
                           barChartBy (420,200) (map (\(h,_,_,lines) -> (show h,lines)) nickHours)
                       unless showrecent $
                         span5 $ do
                           h2 "Active Years (by line)"
                           p $ do
                             barChartBy (300,200) (map (\(year,_,_,_,lines) -> (show year,lines)) nickYears)

                     row $ do
                       span7 $ do
                         h2 "Active Hours (by words)"
                         p $ do
                           radarChart (map (\(h,_,words,_) -> (show h,words)) nickHours)
                           barChartBy (420,200) (map (\(h,_,words,_) -> (show h,words)) nickHours)
                       unless showrecent $
                         span5 $ do
                           h2 "Active Years (by words)"
                           p $ do
                             barChartBy (420,200) (map (\(year,_,_,words,_) -> (show year,words)) nickYears)

                     row $ do
                       span6 $ do
                         h2 "Verbosity (by words/line)"
                         p $ do
                           radarChart (map (\(h,avg,_,_) -> (show h,avg)) nickHours)

    footer

  where nickLines = sum (map (\(_,_,_,_,lines) -> lines) nickYears)
        nickWords = sum (map (\(_,_,_,sum,_) -> sum) nickYears)
        nickAvg   = round (fromIntegral (sum (map (\(_,avg,_,_,_) -> avg) nickYears)) /
                           fromIntegral (length nickYears))

maximumBy' f [] = (0,0,0,0)
maximumBy' f xs = maximumBy f xs
