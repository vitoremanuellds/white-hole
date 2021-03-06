module Entities.Rating where

import Database.PostgreSQL.Simple.FromRow

data Rating = Rating {
    ratingId :: Integer,
    userEmail :: String,
    theId :: Integer,
    rating :: Integer,
    commentary :: String
}

instance FromRow Rating where
    fromRow = Rating <$> field <*> field <*> field <*> field <*> field