{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | Calendar.

module Ircbrowse.View.Calendar
  (calendar
  ,channel
  ,channelNav)
  where

import           Ircbrowse.Types.Import
import           Ircbrowse.View
import           Ircbrowse.View.Template

import           Data.Function
import           Data.List.Split
import qualified Data.Text as T
import           Data.Time.Calendar.OrdinalDate
import           System.Locale

startYear :: Integer
startYear = 2001

channel :: Channel -> Html
channel channel = do
  template "channel" (T.pack ("#" ++ showChan channel)) (return ()) $ do
    channelNav channel
    container $
      row $ span12 $ p "Choose from the menu!"
    footer

calendar :: Day -> Day -> Channel -> Html
calendar firstDay today channel =
  template "calendar" (T.pack ("#" ++ showChan channel)) (return ()) $ do
      channelNav channel
      container $ do
        forM_ (years firstDay today) $ \year ->
          case year of
            ((yearsample:_):_) -> do
               row $
                 span12 $
                   h2 $ toHtml (showYear yearsample)
               forM_ (chunksOf 4 year) $ \months ->
                 row $ do
                   forM_ months $ \days ->
                     span3 $
                       case days of
                         [] -> return ()
                         (monthsample:_) -> do
                           h3 $ toHtml (showMonth monthsample)
                           table $
                             forM_ (chunksOf 7 days) $ \days ->
                               tr $
                                 forM_ days $ \day ->
                                   td $
                                     a ! href (toValue ("/day/" ++ showChan channel ++ "/" ++ showDate day)) $
                                       toHtml (showDayOfMonth day)
            _ -> return ()
      footer

years :: Day -> Day -> [[[Day]]]
years firstDay today =
  reverse (map (groupBy (on (==) showMonth))
               (groupBy (on (==) showYear)
                        (takeWhile (<= today)
                                   (dropWhile (<firstDay)
                                              [fromOrdinalDate startYear 01 ..]))))

showYear :: FormatTime t => t -> String
showYear = formatTime defaultTimeLocale "%Y"

showMonth :: FormatTime t => t -> String
showMonth = formatTime defaultTimeLocale "%B"

showDayOfMonth :: FormatTime t => t -> String
showDayOfMonth = formatTime defaultTimeLocale "%e"

showDate :: FormatTime t => t -> String
showDate = formatTime defaultTimeLocale "%Y/%m/%d"
