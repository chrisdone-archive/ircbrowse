{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.URI
import           Network.URI.Params
import           Prelude (id)
import           Prelude (min)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Links

browse channel = browser True ("Browse #" <> T.pack (showChan channel)) channel $ mempty

pdfs channel = browser False ("PDFs linked in #" <> T.pack (showChan channel)) channel $ do
  p $ a ! A.href (toValue ("/pdfs/" ++ showChan channel ++ "/unique")) $ do
    "Show unique PDF links →"

browser :: Bool -> Text -> Channel -> Html -> URI -> Maybe UTCTime -> [Event] -> PN -> Maybe Text -> Html
browser search title channel extra uri timestamp events pn q =
  template "browse" title mempty $ do
    containerFluid $ do
      mainHeading $
        a ! hrefURI (clearUrlQueries uri) $ toHtml title
      when search $ searchForm q
      extra
      if null events && isJust q
         then noResults
         else paginatedTable channel uri events pn

noResults = do
  p "There are no results for that search!"

searchForm :: Maybe Text -> Html
searchForm q =
  form ! method "get" $
    fieldset $ do
      inputAppend $ do
        input ! name "q" !. "span2" !# "appendedInputButton" ! type_ "text" ! value (maybe "" toValue q)
        input !. "btn" ! type_ "submit" ! value "Go!"

paginatedTable :: Channel -> URI -> [Event] -> PN -> Html
paginatedTable channel uri events pn' = do
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
          nickLink = maybe ""
                           (\nick -> toValue ("/nick/" <> nick))
                           (eventNick event)
      tr ! name (toValue anchor) !# (toValue anchor) !. (toValue (eventClass ++ " " ++ focused)) $ do
        td  !. "timestamp" $ timestamp uri (eventId event) (eventTimestamp event) anchor secs
        if eventType event == "talk"
          then do td !. "nick-wrap" $ do
                    " <"
                    a ! href nickLink !. "nick" ! style color $ toHtml $ fromMaybe " " (eventNick event)
                    "> "
                  td !. "text" $ linkify $ eventText event
          else do td !. "nick-wrap" $
                    a ! href nickLink !. "nick" ! style color $ toHtml $ fromMaybe " " (eventNick event)
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

allPdfs :: URI -> Channel -> [Text] -> Html
allPdfs uri channel lines = do
  template "pdfs" title mempty $ do
    containerFluid $ do
      mainHeading $
        a ! hrefURI (clearUrlQueries uri) $ toHtml title
      p $ a ! A.href (toValue ("/pdfs/" ++ showChan channel)) $ do
          "← Browse all links in context"
      let urls = sortBy (flip (comparing fst))
               $ map (length &&& id)
               $ group
               $ sort
               $ concat
               $ map (filter (isSuffixOf ".pdf") . map show . lefts . explodeLinks)
               $ lines
      table !. "table" $ do
        tr $ do
          th "Linked"
          th "URL"
        forM_ urls $ \(i,url) -> do
          tr $ do
            td (toHtml (show i))
            forM_ (take 1 url) $ \url ->
              td (a ! A.href (toValue url) ! target "_blank" $ toHtml url)

  where title = ("Unique PDFs linked in #" <> T.pack (showChan channel))
