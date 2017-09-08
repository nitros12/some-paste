{-# LANGUAGE QuasiQuotes #-}

module Db ( insertPaste
          , Paste
          ) where

import           GHC.Generics                       (Generic)

import           Data.Int                           (Int64)
import           Data.Maybe                         (listToMaybe)
import           Data.Text                          (Text)
import           Data.Time.Clock

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ   (sql)
import           Database.PostgreSQL.Simple.Time
import           Database.PostgreSQL.Simple.Types


data Paste = Paste { pasteId   :: Int64
                   , lastVisit :: UTCTime
                   , pasteText :: Text
                   , pasteKey  :: Text
                   } deriving Show

instance FromRow Paste where
  fromRow = Paste <$> field <*> field <*> field <*> field

createTable :: Connection -> IO Int64
createTable c = execute_ c [sql|
    CREATE TABLE IF NOT EXISTS pastes (
        paste_id BIGSERIAL PRIMARY KEY,
        last_visit TIMESTAMPTZ,
        paste_text TEXT,
        paste_key VARCHAR(80) UNIQUE)
  |]

insertPaste :: Connection -> Text -> Text -> IO Int64
insertPaste c text key = do
  time <- getCurrentTime
  execute c [sql|
    INSERT INTO pastes
    (paste_id, last_visit, paste_text, paste_key)
    VALUES (?, ?, ?, ?)
  |] (Default, time, text, key)

getPaste :: Connection -> Text -> IO (Maybe Paste)
getPaste c key = do
  time <- getCurrentTime
  query c [sql|
    UPDATE pastes SET last_visit = ?
    WHERE paste_key = ?
    RETURNING *
  |] (time, key) >>= return . listToMaybe

cleanPastes :: Connection -> UTCTime -> IO Int64
cleanPastes c before = execute c [sql|
    DELETE FROM pastes where last_visit < ?
  |] (Only before)

cleanMonthOld :: Connection -> IO Int64
cleanMonthOld c = do
  currentTime <- getCurrentTime
  let delta = fromInteger $ negate 60 * 60 * 24 * 31
  let diff  = addUTCTime delta currentTime
  cleanPastes c diff
