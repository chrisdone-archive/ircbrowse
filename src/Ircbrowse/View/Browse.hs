{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

module Ircbrowse.View.Browse where

import Ircbrowse.View
import Ircbrowse.View.Template

import Data.Text (Text)
import Network.URI
import Network.URI.Params
import Snap.App.Types
import System.Locale
import Data.Time

browse :: URI -> Maybe String -> Maybe String -> Maybe UTCTime -> [Event] -> Pagination -> Maybe Text ->
       Double -> NominalDiffTime -> Html
browse uri network channel timestamp events pagination q secs resecs =
  template "browse" $ do
    div !. "container-fluid" $ do
      h1 $ do
        a ! href "/" $ do "IRC Browse"
        ": "
        maybe (return ()) (\network -> do toHtml network; ": ") network
        a ! hrefURI (clearUrlQueries uri) $ do
          maybe (return ()) (\channel -> do " #"; toHtml channel) channel
      searchForm q
      paginatedTable uri events pagination

searchForm :: Maybe Text -> Html
searchForm q =
  form ! method "get" $
    fieldset $ do
      div !. "input-append" $ do
        input ! name "q" !. "span2" !# "appendedInputButton" ! type_ "text" ! value (maybe "" toValue q)
        button !. "btn" ! type_ "button" $ "Go!"

paginatedTable :: URI -> [Event] -> Pagination -> Html
paginatedTable uri events pagination =
  paginate pagination $
    table !. "events table" $
      forM_ events $ \event -> do
        let secs = formatTime defaultTimeLocale "%s" (zonedTimeToUTC (eventTimestamp event))
            anchor = ("t" ++ secs)
            eventClass | Just t <- lookup "timestamp" (uriParams uri),
                         t == secs = "event info"
                       | otherwise = "event"
            focused | eventType event == "talk" = "focused"
                    | otherwise = "not-focused" :: String
        tr ! name (toValue anchor) !# (toValue anchor) !. (toValue (eventClass ++ " " ++ focused)) $ do
          td  !. "timestamp" $ timestamp uri (eventTimestamp event) anchor secs
          if eventType event == "talk"
            then do td !. "nick-wrap" $ do
                      " <"
                      span !. "nick" $ toHtml $ fromMaybe " " (eventNick event)
                      "> "
                    td !. "text" $ toHtml $ eventText event
            else do td !. "nick-wrap" $
                      span !. "nick" $ toHtml $ fromMaybe " " (eventNick event)
                    td !. "text" $ toHtml $ eventText event


timestamp :: URI -> ZonedTime -> String -> String -> Html
timestamp puri t anchor secs =
  -- a ! hrefURIWithHash uri anchor $ toHtml $ show t
  toHtml $ show t

  where uri = updateUrlParam "timestamp" secs (clearUrlQueries puri)

-- | Render results with pagination.
paginate :: Pagination -> Html -> Html
paginate pn inner = do
  pnnav pn True
  inner
  pnnav pn False

-- | Show a pagination navigation, with results count, if requested.
pnnav :: Pagination -> Bool -> Html
pnnav pn@Pagination{..} showTotal = do
  div !. "row12" $
    ul !. "pager" $ do
        when (pnPage-1 > 0) $ li !. "previous" $ navDirection pn (-1) "← Older"
        toHtml (" " :: Text)
        when (pnResults == pnLimit) $ li !. "next" $ navDirection pn 1 "Newer →"
        when showTotal $ do
          br
          toHtml $ results

    where results = unwords [showCount start ++ "—" ++ showCount end
                            ,"results of"
                            ,showCount pnTotal]
          start = 1 + (pnPage - 1) * pnResults
          end = pnPage * pnResults

-- | Link to change navigation page based on a direction.
navDirection :: Pagination -> Integer -> Text -> Html
navDirection Pagination{..} change caption = do
  a ! hrefURI uri $
    toHtml caption

  where uri = updateUrlParam "page"
  	      		     (show (pnPage + change))
			     (deleteQueryKey "timestamp" pnURI)
