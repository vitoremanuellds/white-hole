module Entities.User where
import Database.PostgreSQL.Simple.FromRow

data User = User {
    email :: String,
    password :: String,
    nome :: String,
    sobrenome :: String
} deriving (Eq, Show, Read)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field