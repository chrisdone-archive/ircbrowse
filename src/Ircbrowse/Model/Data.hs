-- | Generate data needed for statistics and things like that. General
-- data that doesn't need to be generated on-demand.

module Ircbrowse.Model.Data where

import Ircbrowse.Monads
import Snap.App

-- | Generate everything.
generateData :: Model c s ()
generateData = do
  void $ exec ["delete from event_count;"
              ,"insert into event_count select (select count(*) from event where channel = c.id),id from channel c;"]
              ()
