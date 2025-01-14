module Whatbin.Client where

import Prelude

import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Array (head)
import Data.DateTime (Date, date)
import Data.Either (Either, hush, note)
import Data.Formatter.DateTime (unformatDateTime)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Fetch (fetch)

-- C = all, R = recycling
type Response = Array
  { "C1" :: String
  , "C2" :: String
  , "C3" :: String
  , "R1" :: String
  , "R2" :: String
  , "R3" :: String
  }

type ParsedResponse = 
  { recycling :: Array Date
  , all :: Array Date
  }

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
    all <- traverse parseDate [obj."C1", obj."C2", obj."C3"]
    recycling <- traverse parseDate [obj."R1", obj."R2", obj."R3"]
    pure { all, recycling }

  parseDate = unformatDateTime "DD/MM/YYYY" >>> hush <#> map date

