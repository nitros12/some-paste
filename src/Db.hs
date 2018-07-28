{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module Db ( Paste
          , getPaste
          , updateLastVisit
          , insertPaste
          , cleanPastes
          , cleanMonthOld
          , createTable
          , dropTable
          ) where

import           Control.Arrow
import           Data.Int                         (Int64)
import           Data.Maybe                       (listToMaybe)
import           Data.Profunctor.Product          (p5)
import           Data.Text.Lazy                   (Text)
import           Data.Text.Lazy                   as T
import           Data.Time.Clock
import qualified Database.PostgreSQL.Simple       as PQ
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Opaleye

type PasteColumn = (Column PGInt8, Column PGTimestamptz, Column PGText, Column PGText, Column PGText)
type Paste = (Int64, UTCTime, Text, Text, Text)

pasteTable :: Table
              (Maybe (Column PGInt8), Column PGTimestamptz, Column PGText, Column PGText, Column PGText)
              PasteColumn
pasteTable = Table "pastes" (p5 ( optional "id"
                                , required "last_visit"
                                , required "text"
                                , required "key"
                                , required "lang" ))

stripCR :: Text -> Text
stripCR = T.filter (/='\r')

pasteKeyQuery :: Text -> Query PasteColumn
pasteKeyQuery key = proc () -> do
  row@(_, _, _, rkey, _) <- queryTable pasteTable -< ()
  restrict -< (rkey .== constant key)
  returnA -< row

updateLastVisit :: PQ.Connection -> Text -> IO Int64
updateLastVisit c key = do
  time <- getCurrentTime
  runUpdate c pasteTable (update time) predicate
  where
    update time (id_, _, paste, rkey, lang) = (Just id_, constant time, paste, rkey, lang)
    predicate (_, _, _, rkey, _) = rkey .== constant key

getPaste :: PQ.Connection -> Text -> IO (Maybe Paste)
getPaste c key = updateLastVisit c key >> listToMaybe <$> runQuery c (pasteKeyQuery key)


cleanPastes :: PQ.Connection -> UTCTime -> IO Int64
cleanPastes c before = runDelete c pasteTable predicate
  where
    predicate (_, t, _, _, _) = t .< constant before

cleanMonthOld :: PQ.Connection -> IO Int64
cleanMonthOld c = do
  time <- getCurrentTime
  let delta = fromInteger $ negate 60 * 60 * 24 * 31
  let diff  = addUTCTime delta time
  cleanPastes c diff

insertPaste :: PQ.Connection -> Text -> Text -> Text -> IO Int64
insertPaste c paste key lang = do
  let paste' = stripCR paste
  time <- getCurrentTime
  PQ.execute c [sql|
    INSERT INTO "pastes" ("last_visit", "text", "key", "lang")
    VALUES (?, ?, ?, ?)
    ON CONFLICT ("key") DO UPDATE SET "last_visit" = "excluded"."last_visit"
  |] (time, paste', key, lang)

createTable :: PQ.Connection -> IO Int64
createTable c = PQ.execute_ c [sql|
    CREATE TABLE IF NOT EXISTS "pastes" (
        "id" BIGSERIAL PRIMARY KEY,
        "last_visit" TIMESTAMPTZ NOT NULL,
        "text" TEXT NOT NULL,
        "key" TEXT NOT NULL UNIQUE,
        "lang" Text NOT NULL)
  |]

dropTable :: PQ.Connection -> IO Int64
dropTable c = PQ.execute_ c [sql|
    DROP TABLE IF EXISTS "pastes"
  |]
