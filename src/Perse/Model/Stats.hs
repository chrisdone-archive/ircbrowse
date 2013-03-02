module Perse.Model.Stats where

import Perse.Types

import Data.Maybe
import Snap.App

getStats :: Model c s Stats
getStats = do
  count <- singleNoParams ["SELECT COUNT(*)"
                          ,"FROM event"]
  return Stats
    { stEventCount = fromMaybe 0 count
    }
