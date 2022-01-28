{-# LANGUAGE MultiParamTypeClasses #-}

module Ytm.Util.Range where

class Range a v where
  inRange :: v -> a -> Bool
