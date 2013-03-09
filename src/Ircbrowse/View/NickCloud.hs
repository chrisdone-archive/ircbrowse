{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | A word cloud of all the nicknames participating in the time frame.

module Ircbrowse.View.NickCloud where

import Ircbrowse.View
import Ircbrowse.View.Template
import Ircbrowse.View.Cloud

nickCloud :: [(String,Integer)] -> Html
nickCloud stats = do
  template "nick-cloud" cloudScripts $ do
    containerFluid $ do
      row $ do
        span12 $ do
          h1 $ do
            a ! href "/" $ do "IRC Browse"
            ": Nick Word Cloud"
          p $ do "Below is a nick cloud in logarithmic scale, "
                 "because IRC channels tend to have top contributors, "
                 "rather than an evenly distributed user contribution. "
          cloud "cloud-container" (900,700) 450 5 stats
