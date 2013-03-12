{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | Browsing and searching the logs.

module Ircbrowse.View.Browse where

import Ircbrowse.View
import Ircbrowse.View.NickColour
import Ircbrowse.View.Template
import Ircbrowse.System

import Data.Text (Text)
import Network.URI
import Network.URI.Params
import Prelude (min)

browse :: URI -> Maybe String -> Maybe String -> Maybe UTCTime -> [Event] -> PN -> Maybe Text -> Html
browse uri network channel timestamp events pn q =
  template "browse" mempty $ do
    containerFluid $ do
      h1 $ do
        a ! href "/" $ do "IRC Browse"
        ": "
        maybe (return ()) (\network -> do toHtml network; ": ") network
        a ! hrefURI (clearUrlQueries uri) $ do
          maybe (return ()) (\channel -> do " #"; toHtml channel) channel
      searchForm q
      if null events
         then noResults
         else paginatedTable uri events pn

noResults = do
  p "There are no results for that search!"

searchForm :: Maybe Text -> Html
searchForm q =
  form ! method "get" $
    fieldset $ do
      inputAppend $ do
        input ! name "q" !. "span2" !# "appendedInputButton" ! type_ "text" ! value (maybe "" toValue q)
        input !. "btn" ! type_ "submit" ! value "Go!"

paginatedTable :: URI -> [Event] -> PN -> Html
paginatedTable uri events pn' = do
  let pn = pn' { pnURI = deleteQueryKey "timestamp" (deleteQueryKey "id" uri) }
  pagination pn
  table !. "events table" $
    forM_ events $ \event -> do
      let secs = formatTime defaultTimeLocale "%s" (zonedTimeToUTC (eventTimestamp event))
          anchor = ("t" ++ secs)
          eventClass | Just t <- lookup "timestamp" (uriParams uri),
                       t == secs = "event info"
                     | otherwise = "event"
          focused | eventType event `elem` ["talk","act"] = "focused"
                  | otherwise = "not-focused" :: String
          color = toValue (nickColour (fromMaybe "" (eventNick event)))
      tr ! name (toValue anchor) !# (toValue anchor) !. (toValue (eventClass ++ " " ++ focused)) $ do
        td  !. "timestamp" $ timestamp uri (eventId event) (eventTimestamp event) anchor secs
        if eventType event == "talk"
          then do td !. "nick-wrap" $ do
                    " <"
                    span !. "nick" ! style color $ toHtml $ fromMaybe " " (eventNick event)
                    "> "
                  td !. "text" $ linkify $ eventText event
          else do td !. "nick-wrap" $
                    span !. "nick" ! style color $ toHtml $ fromMaybe " " (eventNick event)
                  td !. "text" $ linkify $ eventText event
  pagination pn { pnPn = (pnPn pn) { pnShowDesc = False }
                , pnResultsPerPage = Nothing
                }

timestamp :: URI -> Int -> ZonedTime -> String -> String -> Html
timestamp puri eid t anchor secs =
  a ! hrefURIWithHash uri anchor $ toHtml $ show t

  where uri = updateUrlParam "id" (show eid)
                                  (updateUrlParam "timestamp" secs
                                                  (clearUrlQueries puri))
