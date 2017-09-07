{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Db ( insertPaste
          , Paste
          ) where

import           GHC.Generics                       (Generic)

import           Data.Int                           (Int64)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ   (sql)
import           Database.PostgreSQL.Simple.Types

import Data.Maybe (listToMaybe)
import           Data.Text                          (Text)

data Paste = Paste { paste_id   :: Int64
                   , paste_text :: Text
                   , paste_key  :: Text
                   } deriving Show

instance FromRow Paste where
  fromRow = Paste <$> field <*> field <*> field

insertPaste :: Connection -> Text -> Text -> IO Int64
insertPaste c text key = execute c [sql|
    INSERT INTO pastes
    (paste_id, paste_text, paste_key)
    VALUES (?, ?, ?)
  |] (Default, text, key)

getPaste :: Connection -> Text -> IO (Maybe Paste)
getPaste c key = query c [sql|
    SELECT * FROM pastes WHERE pastes.paste_key = ?
  |] (Only key) >>= return . listToMaybe
