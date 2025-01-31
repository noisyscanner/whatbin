module Whatbin.Client where

import Prelude

import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head, sort)
import Data.Bifunctor (lmap)
import Data.DateTime (Date, date)
import Data.Either (Either, hush, note)
import Data.Formatter.DateTime (unformatDateTime)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Fetch (fetch)

data Bin = All | Recycling

derive instance Eq Bin

printBin :: Bin -> String
printBin All = "All bins"
printBin Recycling = "Recycling"

data BinDate = BinDate Bin Date

derive instance Eq BinDate
instance Ord BinDate where
  compare (BinDate _ a) (BinDate _ b) = compare a b

-- C = all, R = recycling
type Response = Array
  { "C1" :: String
  , "C2" :: String
  , "C3" :: String
  , "R1" :: String
  , "R2" :: String
  , "R3" :: String
  }

type ParsedResponse = Array BinDate

decodeResponse :: Json -> Either String Response
decodeResponse = decodeJson >>> lmap show

url :: String
url = "https://webapps.southglos.gov.uk/Webservices/SGC.RefuseCollectionService/RefuseCollectionService.svc/getCollections/674321"

getBin :: Aff (Either String ParsedResponse)
getBin = do
  eitherRes <- fetch url {} >>= _.text <#> jsonParser
  pure do eitherRes >>= decodeResponse >>= parseResponse

  where

  parseResponse :: Response -> Either String ParsedResponse
  parseResponse json = note "No results" do
    obj <- head json
    all <- traverse parseDate [ obj."C1", obj."C2", obj."C3" ]
    recycling <- traverse parseDate [ obj."R1", obj."R2", obj."R3" ]

    let allDates = (all <#> BinDate All) <> (recycling <#> BinDate Recycling)

    pure $ sort allDates

  parseDate = unformatDateTime "DD/MM/YYYY" >>> hush <#> map date

