module Entities.Movie where

import Database.PostgreSQL.Simple.FromRow
import Data.Time

data Movie = Movie {
    movieId :: Integer,
    title :: String,
    releaseDate :: String,
    summary :: String,
    duration :: String,
    rating :: Float
} deriving (Show, Read, Eq)

instance FromRow Movie where
    fromRow = Movie <$> field <*> field <*> field <*> field <*> field <*> field

