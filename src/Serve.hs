{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Serve where

import           Control.Monad.Reader          (MonadIO, MonadReader, ReaderT,
                                                asks, lift, liftIO, when)
import           Data.Digest.XXHash            (xxHash')
import           Data.Int                      (Int32, Int64)
import           Data.Pool                     (Pool, withResource)
import qualified Data.Text                     as ST
import           Data.Text.Encoding            (encodeUtf8)
import           Data.Text.Format              (format)
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy                as T
import           Data.Word                     (Word16)
import           Database.PostgreSQL.Simple    (Connection)
import           Db
import           GHC.Generics
import           Network.HTTP.Types.Status     (Status, status404)
import           System.Envy
import           Templates
import           Text.Blaze.Html.Renderer.Text
import           Web.Scotty.Trans              (ActionT, Parsable, html, param,
                                                params, parseParam, raise,
                                                redirect, status, text)

data Config = Config { port       :: Int    -- "PORT"
                     , maxLength  :: Int64  -- "MAX_LENGTH"
                     , pgHost     :: String -- "PG_HOST"
                     , pgPort     :: Word16 -- "PG_PORT"
                     , pgUser     :: String -- "PG_USER"
                     , pgPass     :: String -- "PG_PASS"
                     , pgDatabase :: String -- "PG_DATABASE"
                     } deriving (Generic, Show)

instance DefConfig Config where
  defConfig = Config 3000 20000 "localhost" 5432 "postgres" "" "postgres"

instance FromEnv Config

data AppState = AppState { config   :: Config
                         , connPool :: Pool Connection
                         }

newtype AppStateM a = AppStateM { runAppStateM :: ReaderT AppState IO a
                                } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

type ActionC = ActionT Text AppStateM

hashPaste :: ST.Text -> Int32
hashPaste = fromIntegral . xxHash' . encodeUtf8

errorWith :: Status -> Text -> ActionC ()
errorWith code reason = status code >> text reason

maybeParam :: Parsable a => a -> Text -> ActionC a
maybeParam d key = do
  paramList <- params
  case lookup key paramList of
    Just a  -> return $ either (const d) id $ parseParam a
    Nothing -> return d

retrievePaste :: ActionC ()
retrievePaste = do
  key <- param "key"
  theme <- maybeParam "plain" "theme"
  pool <- lift $ asks connPool
  paste <- liftIO $ withResource pool (`getPaste` key)
  case paste of
    Just p  ->  html . renderHtml $ viewPaste p theme
    Nothing -> errorWith status404 $ format "Paste {} not found" [key]

savePaste :: ActionC ()
savePaste = do
  max_length <- lift (maxLength <$> asks config)
  lang <- maybeParam "plain" "lang"
  paste <- param "text"
  when (T.length paste > max_length) $ raise (format "Paste over length: {}" [max_length])
  let key = hashPaste $ T.toStrict (T.append paste lang)
  pool <- lift $ asks connPool
  liftIO $ withResource pool (\conn -> insertPaste conn paste key lang)
  redirect (format "/paste/{}" [key])

retrievePasteRaw :: ActionC ()
retrievePasteRaw = do
  key <- param "key"
  pool <- lift $ asks connPool
  paste <- liftIO $ withResource pool (`getPaste` key)
  case paste of
    Just p  -> text . plainPaste $ p
    Nothing -> errorWith status404 $ format "Paste {} not found" [key]

pageIndex :: ActionC ()
pageIndex = html . renderHtml $ frontPage
