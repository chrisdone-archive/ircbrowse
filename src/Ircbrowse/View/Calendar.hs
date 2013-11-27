{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | Calendar.

module Ircbrowse.View.Calendar
  (calendar)
  where

import Ircbrowse.Types.Import
import Ircbrowse.View
import Ircbrowse.View.Chart
import Ircbrowse.View.Cloud
import Ircbrowse.View.Template

import Data.Text (Text)
import qualified Data.Text as T
import Data.List.Split
import Control.Arrow
import Data.Function
import Data.Time
import Data.Time.Calendar.OrdinalDate
import System.Locale

startYear = 2001

calendar :: Day -> Day -> Channel -> Html
calendar firstDay today channel = do
  let title = do "Calendar: "
                 a ! href (toValue ("/browse/" ++ showChan channel)) $
                   toHtml (T.pack ("#" ++ showChan channel))
  template "calendar" (T.pack ("#" ++ showChan channel)) (return ()) $ do
    container $ do
      mainHeading $ title
      p $ a ! href (toValue ("/browse/" ++ showChan channel)) $ do
        "Browse and search all "; toHtml ("#" ++ showChan channel); " logs →"
      p $ a ! href (toValue ("/day/" ++ showChan channel ++ "/today")) $ do
        "View today's complete log →"
      p $ a ! href (toValue ("/day/" ++ showChan channel ++ "/today?mode=recent")) $ do
        "View recent logs from today →"
      forM_ (years firstDay today) $ \year ->
        case year of
          [] -> return ()
          ((yearsample:_):_) -> do
             row $
               span12 $
                 h2 $ toHtml (showYear yearsample)
             forM_ (chunk 4 year) $ \months ->
               row $ do
                 forM_ months $ \days ->
                   span3 $
                     case days of
                       [] -> return ()
                       (monthsample:_) -> do
                         h3 $ toHtml (showMonth monthsample)
                         table $
                           forM_ (chunk 7 days) $ \days ->
                             tr $
                               forM_ days $ \day ->
                                 td $
                                   a ! href (toValue ("/day/" ++ showChan channel ++ "/" ++ showDate day)) $
                                     toHtml (showDayOfMonth day)
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
