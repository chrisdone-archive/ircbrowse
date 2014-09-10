{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | Display a profile of the given nick.

module Ircbrowse.View.Profile

  where

import           Ircbrowse.Model.Profile
import           Ircbrowse.Types.Import
import           Ircbrowse.View
import           Ircbrowse.View.Chart
import           Ircbrowse.View.Template

import           Data.Text (Text)
import qualified Data.Text as T
import           System.Locale
import           Text.Printf

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
            _ -> return ()
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

profileSections :: Text -> Bool -> NickStats -> Html
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

summary :: ToMarkup a
        => a -> Bool -> NickStats -> Html
summary nick showrecent NickStats{..} = do
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
  p $ do toHtml nick; " has a karma of "; toHtml (show nickKarma)

  where nickWords = sum (map (\(_,_,_,sum,_) -> sum) nickYears)
        nickAvg   = round (fromIntegral (sum (map (\(_,avg,_,_,_) -> avg) nickYears)) /
                           fromIntegral (length nickYears))

maximumBy'
  :: (Num t, Num t1, Num t2, Num t3) =>
     ((t, t1, t2, t3) -> (t, t1, t2, t3) -> Ordering)
     -> [(t, t1, t2, t3)] -> (t, t1, t2, t3)
maximumBy' _ [] = (0,0,0,0)
maximumBy' f xs = maximumBy f xs

logsLink :: Text -> Html
logsLink nick = do
  "Search logs for this nick: "
  htmlCommas $ flip map [toEnum 0 ..] $ \chan ->
    a ! href (toValue ("/browse/" <> T.pack (showChan chan) <> "?q=" <> nick)) $
      toHtml $ "#" ++ showChan chan
