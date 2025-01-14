module Main where

import Prelude

import Data.Traversable (traverse_)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Whatbin.Client (getBin)

main :: Effect Unit
main = launchAff_ do
  res <- getBin
  traverse_ traceM res
  pure unit

