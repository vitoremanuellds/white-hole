module Entities.Serie where

import Database.PostgreSQL.Simple.FromRow

data Serie = Serie {
    serieId :: Integer,
    title :: String,
    releaseDate :: String,
    episodes :: String,
    summary :: String,
    rating :: Float
} deriving (Show, Read, Eq)

instance FromRow Serie where
    fromRow = Serie <$> field <*> field <*> field <*> field <*> field <*> field