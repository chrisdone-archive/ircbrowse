-- | Convenient operator.

module Data.Monoid.Operator where

import Data.Monoid

(++) :: (Monoid a) => a -> a -> a
(++) = mappend