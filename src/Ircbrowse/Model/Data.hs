-- | Generate data needed for statistics and things like that. General
-- data that doesn't need to be generated on-demand.

module Ircbrowse.Model.Data where

import Ircbrowse.Monads
import Snap.App

-- | Generate everything.
generateData :: Model c s ()
generateData = do
  void $ exec ["update event_count set count = (select count(*) from event)"] ()
