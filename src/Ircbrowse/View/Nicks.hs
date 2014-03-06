{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | A word cloud of all the nicknames participating in the time frame.

module Ircbrowse.View.Nicks where

import Ircbrowse.View
import Ircbrowse.Types.Import
import Ircbrowse.View.Template
import Ircbrowse.View.Cloud

import qualified Data.Text as T

nicks channel count ns mode = do
  template "nicks-page" ("Top " <> T.pack (showCount (length ns)) <> " Nicks") mempty $ do
    channelNav channel
    container $ do
      row $
        span12 $
          p $ do "There have been many to enter the grizzly and chocolate flavoured world of IRC. "
                 if mode == "all" then "Over the years, " else "Over the past month, "
                 "there have been "; toHtml (showCount count) ; " nick names represented in this channel, ";
                 " who were worthy enough to be listed in this glorious shrine to those "
                 " wonderful participants who just couldn't shut up."
      row $
        span6 $ nicklist channel ns mode
    footer

nicklist c ns mode = do
  tabNav $ do
    li !. (if mode == "recent" then "active" else "") $ a ! href (link "recent") $ "Recent"
    li !. (if mode == "all" then "active" else "") $ a ! href (link "all") $ "All Time"
  table !. "table" $ do
    thead $
      tr $ do th "Nick"
              th "Messages"
    tbody $
      forM_ ns $ \(nick,count) -> do
        tr $ do
          td $ a ! href (toValue ("/nick/" <> nick)) $ toHtml nick
          td $ toHtml (showCount count)
  where link n = toValue ("/nicks/" ++ showChan c ++ "/" ++ n)

showChans =
  htmlCommasAnd $ flip map [toEnum 0 ..] $ \chan ->
    a ! href (toValue ("/browse/" <> T.pack (showChan chan))) $
      toHtml $ "#" ++ showChan chan
