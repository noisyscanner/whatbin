module Main where

import Prelude

import Data.Date (day, month)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Whatbin.Client (BinDate(..), getBin, printBin)

printBinDate :: BinDate -> String
printBinDate (BinDate bin date) = do
  let
    d = fromEnum $ day date
    m = fromEnum $ month date
  printBin bin <> " - " <> show d <> "/" <> show m

main :: Effect Unit
main = launchAff_ do
  res <- getBin
  case res of
    Left err -> liftEffect $ Console.error err
    Right binDates -> liftEffect do
      traverse_ (printBinDate >>> Console.log) binDates

