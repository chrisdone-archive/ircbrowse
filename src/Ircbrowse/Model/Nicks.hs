-- | Statistics for nicks in aggregate.

module Ircbrowse.Model.Nicks where

import Ircbrowse.Types
import Ircbrowse.Data
import Ircbrowse.Tunes

import Snap.App
import Data.Text (Text)

getNicks :: Int -> Bool -> Range -> Model c s [(Text,Int)]
getNicks n recent (Range from to) = do
  query ["SELECT nick,COUNT(*)"
        ,"FROM event"
        ,"WHERE TYPE in ('talk','act') AND"
        ,"(NOT ? OR (timestamp > ? AND timestamp < ?)) AND"
        ,"NOT (text LIKE '@%' OR text LIKE '> %' OR text LIKE ':t %' OR text LIKE ':k %'"
        ,"OR text LIKE 'lambdabot: %')"
        ,"GROUP BY nick"
        ,"ORDER BY 2 desc"
        ,"LIMIT ?;"]
        (recent,from,to,n)

getNickCount :: Bool -> Range -> Model c s Int
getNickCount recent (Range from to) = do
  fmap (sum . map (\(Only x) -> x))
       (query ["SELECT DISTINCT COUNT(*)"
              ,"FROM (SELECT nick FROM event"
              ,"      WHERE (NOT ? OR (timestamp > ? AND timestamp < ?))"
              ,"      GROUP BY nick) c"]
              (recent,from,to))
