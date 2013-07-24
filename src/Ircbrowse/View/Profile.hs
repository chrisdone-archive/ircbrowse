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

import System.Locale
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow
import Text.Printf

nickProfile :: Text -> Bool -> NickStats -> Html
nickProfile nick showrecent ns@NickStats{..} = do
  template "nick-profile" nick mempty $ do
    container $ do
      mainHeading $ toHtml nick
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
             then do p $ do toHtml nick; " hasn't said anything in the past month."
                     logsLink nick
             else profileSections nick showrecent ns
    footer

profileSections nick showrecent ns@NickStats{..} =
  do summary nick showrecent ns
     logsLink nick
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
           forM_ nickQuotes $ \(idx,time,quote) -> do
             blockquote $ do
               toHtml quote
               " — "
               toHtml $
                 a ! href (toValue (makeLink idx time)) $ toHtml $ show time

  where verbosity = span5 $ do
                      h2 "Verbosity (by words/line)"
                      p $ do
                        radarChart (map (\(h,avg,_,_) -> (show h,avg)) nickHours)
        makeLink eid t =
          concat ["http://ircbrowse.net/browse/haskell?id="
                 ,show eid
                 ,"&timestamp="
                 ,secs
                 ,"#t"
                 ,secs]
         where secs = formatTime defaultTimeLocale "%s" t

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

logsLink :: Text -> Html
logsLink nick = do
  "Search logs for this nick: "
  htmlCommas $ flip map [toEnum 0 ..] $ \chan ->
    a ! href (toValue ("/browse/" <> T.pack (showChan chan) <> "?q=" <> nick)) $
      toHtml $ "#" ++ showChan chan
