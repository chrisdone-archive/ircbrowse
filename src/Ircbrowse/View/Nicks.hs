{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | A word cloud of all the nicknames participating in the time frame.

module Ircbrowse.View.Nicks where

import Ircbrowse.View
import Ircbrowse.Tunes
import Ircbrowse.View.Template
import Ircbrowse.View.Cloud

import qualified Data.Text as T

nicks count ns showrecent = do
  template "nicks-page" ("Top " <> T.pack (showCount (length ns)) <> " Nicks") mempty $ do
    container $ do
      mainHeading $ do "Top "; toHtml (showCount (length ns)); " Nicks"
      row $
        span12 $
          p $ do "There have been many to enter the grizzly and chocolate flavoured world of IRC. "
                 if not showrecent then "Over the years, " else "Over the past month, "
                 "there have been "; toHtml (showCount count) ; " nick names represented in the channels ";
                 showChans; ", but of those, these are the "; toHtml (showCount (length ns));
                 " who were worthy enough to be listed in this glorious shrine to those "
                 " wonderful participants who just couldn't shut up."
      row $
        span6 $ nicklist ns showrecent
    footer

nicklist ns showrecent = do
  tabNav $ do
    li !. (if showrecent then "active" else "") $ a ! href "?recent=true" $ "Recent"
    li !. (if not showrecent then "active" else "") $ a ! href "?recent=false" $ "All Time"
  table !. "table" $ do
    thead $
      tr $ do th "Nick"
              th "Messages"
    tbody $
      forM_ ns $ \(nick,count) -> do
        tr $ do
          td $ a ! href (toValue ("/nick/" <> nick)) $ toHtml nick
          td $ toHtml (showCount count)

showChans =
  htmlCommasAnd $ flip map [toEnum 0 ..] $ \chan ->
    a ! href (toValue ("/browse/" <> T.pack (showChan chan))) $
      toHtml $ "#" ++ showChan chan
