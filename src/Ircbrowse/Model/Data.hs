-- | Generate data needed for statistics and things like that. General
-- data that doesn't need to be generated on-demand.

module Ircbrowse.Model.Data where

import Ircbrowse.Monads
import Snap.App

-- | Generate everything.
generateData :: Model c s ()
generateData = do
  void $ do
         exec ["delete from event_count;"
              ,"insert into event_count select (select count(*) from event where channel = c.id),id from channel c;"]
              ()
	 exec ["delete from conversation_by_year"] ()
	 exec ["insert into conversation_by_year select date_part('year',timestamp),count(*) from event where type in ('talk','act') group by date_part('year',timestamp) order by 1;"] ()
	 exec ["delete from general_activity_by_year"] ()
	 exec ["insert into general_activity_by_year select date_part('year',timestamp),count(*) from event group by date_part('year',timestamp) order by 1;"] ()
