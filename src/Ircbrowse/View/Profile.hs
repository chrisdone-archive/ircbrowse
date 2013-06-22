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

import Data.Text (Text)
import Control.Arrow
import Text.Printf

nickProfile :: Text -> NickStats -> Html
nickProfile nick NickStats{..} = do
  template "nick-profile" mempty $ do
    container $ do
      h1 (toHtml nick)
      p $ do toHtml nick
             " has written "
             toHtml $ showCount nickLines
             " lines in "
             toHtml $ show (length nickYears)
             " years, most active at around "
             toHtml $ (printf "%.0d" (fst (maximumBy (comparing snd) nickHours)) :: String)
             ":00 (UTC)."
      h2 "Active Hours"
      barChart (map (first show) nickHours)
    footer
