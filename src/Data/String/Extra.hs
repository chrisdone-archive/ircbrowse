module Data.String.Extra where

import Data.Char

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

limitLen :: Int -> [Char] -> [Char]
limitLen n xs | length xs > n = take (max 1 (n-2)) xs ++ "…"
              | otherwise     = xs
