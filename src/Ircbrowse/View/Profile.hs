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
nickProfile nick showrecent ns@NickStats{..} = do
  template "nick-profile" (H.title (toHtml nick)) $ do
    container $ do
      h1 (toHtml nick)
      case nickQuote of
        Nothing -> mempty
        Just (typ,quote) -> blockquote $
          case typ of
            "act" -> em (do toHtml nick; " "; toHtml quote)
            "talk" -> do em (toHtml ("“" <> quote <> "”")); " — "; toHtml nick
      tabNav $ do
        li !. (if showrecent then "active" else "") $ a ! href "?recent=true" $ "Recent"
        li !. (if not showrecent then "active" else "") $ a ! href "?recent=false" $ "All Time"
      row $
        span12 $
          if nickLines == 0
             then p $ do toHtml nick; " hasn't said anything in the past month."
             else profileSections nick showrecent ns
    footer

profileSections nick showrecent ns@NickStats{..} =
  do summary nick showrecent ns
     row $ do
       span7 $ do
         h2 "Active Hours (by line)"
         p $ do
           radarChart (map (\(h,_,_,lines) -> (show h,lines)) nickHours)
           barChartBy (420,200) (map (\(h,_,_,lines) -> (show h,lines)) nickHours)
       if showrecent
          then verbosity
          else span5 $ do
                 h2 "Active Years (by line)"
                 p $ do
                   barChartBy (420,200) (map (\(year,_,_,_,lines) -> (show year,lines)) nickYears)

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

     unless showrecent $
       row $ do
         verbosity

     unless (null nickQuotes) $
       row $
         span12 $ do
           h2 "Memorable Quotes"
           forM_ nickQuotes $ \quote -> do
             blockquote $ toHtml quote

  where verbosity = span5 $ do
                      h2 "Verbosity (by words/line)"
                      p $ do
                        radarChart (map (\(h,avg,_,_) -> (show h,avg)) nickHours)

summary nick showrecent NickStats{..} =
  p $ do toHtml nick; " has written "
         toHtml (showCount nickWords); " words, "
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

  where nickWords = sum (map (\(_,_,_,sum,_) -> sum) nickYears)
        nickAvg   = round (fromIntegral (sum (map (\(_,avg,_,_,_) -> avg) nickYears)) /
                           fromIntegral (length nickYears))

maximumBy' f [] = (0,0,0,0)
maximumBy' f xs = maximumBy f xs
