{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | Browsing and searching the logs.

module Ircbrowse.View.Browse where

import           Ircbrowse.System
import           Ircbrowse.Types.Import
import           Ircbrowse.View hiding (id)
import           Ircbrowse.View.NickColour
import           Ircbrowse.View.Template

import           Control.Arrow
import           Data.Either
import           Data.Function
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.URI
import           Network.URI.Params
-- import           Prelude (id)
-- import           Prelude (min)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Links

browse :: Channel -> URI -> Maybe UTCTime -> [Event] -> PN -> Maybe Text -> Html
browse channel = browser True ("Browse #" <> T.pack (showChan channel)) channel $ mempty

pdfs :: Channel -> URI -> Maybe UTCTime -> [Event] -> PN -> Maybe Text -> Html
pdfs channel = browser False ("PDFs linked in #" <> T.pack (showChan channel)) channel $ do
  p $ a ! A.href (toValue ("/pdfs/" ++ showChan channel ++ "/unique")) $ do
    "Show unique PDF links →"

browser :: Bool
        -> Text
        -> Channel
        -> Html
        -> URI
        -> Maybe UTCTime
        -> [Event]
        -> PN
        -> Maybe Text
        -> Html
browser search title' channel extra uri _ events pn q' =
  template "browse" title' mempty $ do
    channelNav channel
    containerFluid $ do
      when search $ searchForm q'
      extra
      if null events && isJust q'
         then noResults
         else paginatedTable channel uri events pn
    footer
    script ! src "http://code.jquery.com/jquery-2.1.1.min.js" $ mempty
    script ! src "/js/link.js" $ mempty

selection :: Channel -> Text -> [Event] -> URI -> Html
selection channel title' events uri =
  template "browse"
           title'
           mempty
           (do channelNav channel
               containerFluid (do h1 (toHtml title')
                                  eventsTable False events uri)
               footer)

browseDay :: Channel -> Text -> Text -> [Event] -> URI -> Html
browseDay channel current date events uri = do
  template "browse" date mempty $ do
    channelNav channel
    containerFluid $ do
      h1 $ toHtml date
      if null events
         then noResults
         else do when (current == "recent")
                      (do p "Newest at the top")
                 eventsTable True events uri
    footer
  when (current == "recent") $
    do script ! src "/js/recent.js" $ mempty
  script ! src "http://code.jquery.com/jquery-2.1.1.min.js" $ mempty
  script ! src "/js/link.js" $ mempty

noResults :: Html
noResults = do
  p "There are no results!"

searchForm :: Maybe Text -> Html
searchForm q' =
  form ! method "get" $
    fieldset $ do
      inputAppend $ do
        input ! name "q" !. "span2" !# "appendedInputButton" ! type_ "text" ! value (maybe "" toValue q') ! placeholder "(the search index is updated once daily)" ! A.style "width:27em"
        input !. "btn" ! type_ "submit" ! value "Go!"


paginatedTable :: Channel -> URI -> [Event] -> PN -> Html
paginatedTable _ uri events pn' = do
  let pn = pn' { pnURI = deleteQueryKey "timestamp" (deleteQueryKey "id" uri) }
  pagination pn
  eventsTable True events uri
  pagination pn { pnPn = (pnPn pn) { pnShowDesc = False }
                , pnResultsPerPage = Nothing
                }

eventsTable :: Bool -> [Event] -> URI -> Html
eventsTable clear events uri =
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
          nickLink = maybe ""
                           (\nick -> toValue ("/nick/" <> nick))
                           (eventNick event)
      tr ! name (toValue anchor) !# (toValue anchor) !. (toValue (eventClass ++ " " ++ focused)) $ do
        td !# (toValue ("id" <> show (eventId event))) !. "timestamp" $ timestamp clear uri (eventId event) (eventTimestamp event) anchor secs
        if eventType event == "talk"
          then do td !. "nick-wrap" $ do
                    " <"
                    a ! href nickLink !. "nick" ! style color $ toHtml $ fromMaybe " " (eventNick event)
                    "> "
                  td !. "text" $ linkify $ eventText event
          else do td !. "nick-wrap" $
                    a ! href nickLink !. "nick" ! style color $ toHtml $ fromMaybe " " (eventNick event)
                  td !. "text" $ linkify $ eventText event

timestamp :: Bool -> URI -> Int -> ZonedTime -> String -> String -> Html
timestamp clear puri eid t anchor secs =
  a ! hrefURIWithHash uri anchor $ toHtml $ show t

  where uri = updateUrlParam "id" (show eid)
                                  (updateUrlParam "timestamp" secs
                                                  (if clear
                                                      then clearUrlQueries puri
                                                      else puri))

allPdfs :: URI -> Channel -> [(Int,ZonedTime,Text)] -> Html
allPdfs uri channel lines' = do
  template "pdfs" title' mempty $ do
    containerFluid $ do
      mainHeading $
        a ! hrefURI (clearUrlQueries uri) $ toHtml title'
      p $ a ! A.href (toValue ("/pdfs/" ++ showChan channel)) $ do
          "← Browse all links in context"
      let urls = sortBy (flip (comparing fst))
               $ map (length &&& id)
               $ groupBy (on (==) (\(_,_,url) -> url))
               $ sortBy (comparing (\(_,_,url) -> url))
               $ concat
               $ map (\(id',time',text) -> map (id',time',) . filter (isSuffixOf ".pdf") . map show . lefts . explodeLinks $ text)
               $ lines'
      table !. "table" $ do
        tr $ do
          th "Linked"
          th "URL"
        forM_ urls $ \(i,urls') -> do
          tr $ do
            td (toHtml (show i))
            forM_ (take 1 urls') $ \(_,_,url) ->
              td $ do
                a ! A.href (toValue url) ! target "_blank" $ toHtml url
                " — "
                a ! A.href (toValue ("http://ircbrowse.net/browse/" ++ showChan channel ++ "?q=" ++ url)) $
                  "Search results"
                " — Context"; (if length urls' == 1 then "" else "s"); ": "
                forM_ (take 30 (zip [1..] urls')) $ \(j,(i',t,_)) -> do
                  let secs = formatTime defaultTimeLocale "%s" (zonedTimeToUTC t)
                  a ! A.href (toValue ("/browse/" ++ showChan channel ++ "?id=" ++ show i' ++
                                      "&timestamp=" ++ secs ++ "#t" ++ secs)) $
                   toHtml (show j)
                  " "
                when (length urls' > 30) $ " …"

  where title' = ("Unique PDFs linked in #" <> T.pack (showChan channel))
