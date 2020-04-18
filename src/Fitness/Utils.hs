{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Fitness.Utils
    ( pairs
    , appendItemToListInMap
    ) where

import qualified Data.Map.Lazy as M

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

appendItemToListInMap :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a]
appendItemToListInMap key value map' =
  case (map' M.!? key) of
    Just vs -> M.insert key (value:vs) map'
    Nothing -> M.insert key [value] map'
