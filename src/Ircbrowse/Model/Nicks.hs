-- | Statistics for nicks in aggregate.

module Ircbrowse.Model.Nicks where

import Ircbrowse.Data
import Ircbrowse.Types
import Ircbrowse.Types.Import

import Snap.App
import Data.Text (Text)

getNicks :: Channel -> Int -> Bool -> Range -> Model c s [(Text,Int)]
getNicks channel n recent (Range from to) = do
  query ["SELECT nick,COUNT(*)"
        ,"FROM event"
        ,"WHERE channel = ? and TYPE in ('talk','act') AND"
        ,"(NOT ? OR (timestamp > ? AND timestamp < ?)) AND"
        ,"NOT (text LIKE '@%' OR text LIKE '> %' OR text LIKE ':t %' OR text LIKE ':k %'"
        ,"OR text LIKE 'lambdabot: %')"
        ,"GROUP BY nick"
        ,"ORDER BY 2 desc"
        ,"LIMIT ?;"]
        (showChanInt channel,recent,from,to,n)

getNickCount :: Channel -> Bool -> Range -> Model c s Int
getNickCount channel recent (Range from to) = do
  fmap (sum . map (\(Only x) -> x))
       (query ["SELECT DISTINCT COUNT(*)"
              ,"FROM (SELECT nick FROM event"
              ,"      WHERE channel= ? and (NOT ? OR (timestamp > ? AND timestamp < ?))"
              ,"      GROUP BY nick) c"]
              (showChanInt channel,recent,from,to))
