module Ytm.Util.List where

import Data.Bifunctor (second)
import Data.Maybe (fromJust, isJust)

merge :: (a -> Maybe b -> c) -> (a -> b -> Bool) -> [a] -> [b] -> [c]
merge mf ff as bs = zipWith mf na nb
  where
    (na, nb) = unzip abs'
    abs' = map (\a -> (a, find a bs)) as
    find e ls = case filter (ff e) ls of
      h : _ -> Just h
      _ -> Nothing

mergeDropUnmatched :: (a -> b -> c) -> (a -> b -> Bool) -> [a] -> [b] -> [c]
mergeDropUnmatched mf ff as bs = zipWith mf na nb
  where
    (na, nb) = unzip abs'
    abs' = map (second fromJust) . filter (isJust . snd) . map (\a -> (a, find a bs)) $ as
    find e ls = case filter (ff e) ls of
      h : _ -> Just h
      _ -> Nothing
