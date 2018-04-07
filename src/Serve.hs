{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Serve ( Config (..)
             , AppState (..)
             , AppStateM (..)
             , ActionC (..)
             , maybeParam
             , retrievePaste
             , savePaste
             , retrievePasteRaw
             , pageIndex
             , pageAbout
             , getTheme
             , getBaseStyle
             ) where

import           BasicPrelude                  hiding (Text, encodeUtf8)
import           Control.Monad.Reader          (MonadIO, MonadReader, ReaderT,
                                                asks, lift, liftIO, void, when)
import           Data.ByteString               (ByteString)
import           Data.Digest.XXHash            (xxHash')
import           Data.Int                      (Int32, Int64)
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>))
import           Data.Pool                     (Pool, withResource)
import qualified Data.Text                     as ST
import           Data.Text.Encoding            (encodeUtf8)
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Encoding       as DLT
import           Data.Word                     (Word16)
import           Database.PostgreSQL.Simple    (Connection)
import           Db
import           GHC.Generics
import           Highlight                     (getStyleCss)
import           Network.HTTP.Types.Status     (Status, status404)
import           System.Envy
import           Templates
import           Text.Blaze.Html.Renderer.Text
import           Web.Scotty.Trans              (ActionT, Parsable, addHeader,
                                                html, param, params, parseParam,
                                                raise, raw, redirect, setHeader,
                                                status, text)

data Config = Config
  { host       :: ByteString -- "HOST"
  , port       :: Int -- "PORT"
  , maxLength  :: Int64 -- "MAX_LENGTH"
  , pgHost     :: String -- "PG_HOST"
  , pgPort     :: Word16 -- "PG_PORT"
  , pgUser     :: String -- "PG_USER"
  , pgPass     :: String -- "PG_PASS"
  , pgDatabase :: String -- "PG_DATABASE"
  } deriving (Generic, Show)

instance DefConfig Config where
  defConfig = Config "localhost" 3000 1000000 "localhost" 5432 "postgres" "" "postgres"

instance FromEnv Config

data AppState = AppState
  { config   :: Config
  , connPool :: Pool Connection
  }

newtype AppStateM a = AppStateM
  { runAppStateM :: ReaderT AppState IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

type ActionC = ActionT Text AppStateM

tshow' = T.fromStrict . tshow

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

hashPaste :: ST.Text -> Int32
hashPaste = fromIntegral . xxHash' . encodeUtf8

errorWith :: Status -> Text -> ActionC ()
errorWith code reason = status code >> text reason

canCache :: ActionC ()
canCache = addHeader "Cache-Control" "max-age=31536000"

maybeParam :: Parsable a => a -> Text -> ActionC a
maybeParam d key = do
  paramList <- params
  return . fromMaybe d $ lookup key paramList >>= rightToMaybe . parseParam

retrievePaste :: ActionC ()
retrievePaste = do
  canCache
  key <- param "key"
  theme <- maybeParam "plain" "theme"
  pool <- lift $ asks connPool
  paste <- liftIO $ withResource pool (`getPaste` key)
  case paste of
    Just p  -> html . renderHtml $ viewPaste p theme
    Nothing -> errorWith status404 $ "Paste " <> tshow' key <> " not found"

savePaste :: ActionC ()
savePaste = do
  max_length <- lift (maxLength <$> asks config)
  lang <- maybeParam "plain" "lang"
  paste <- param "text"
  when (T.length paste > max_length) $
    raise $ "Paste over length: " <> (tshow' . fromIntegral $ max_length)
  let key = hashPaste $ T.toStrict (T.append paste lang)
  pool <- lift $ asks connPool
  void . liftIO $ withResource pool (\conn -> insertPaste conn paste key lang)
  redirect $ "/paste/" <> tshow' key

retrievePasteRaw :: ActionC ()
retrievePasteRaw = do
  canCache
  key <- param "key"
  pool <- lift $ asks connPool
  paste <- liftIO $ withResource pool (`getPaste` key)
  case paste of
    Just p  -> text . plainPaste $ p
    Nothing -> errorWith status404 $ "Paste " <> tshow' key <> " not found"

pageIndex :: ActionC ()
pageIndex = do
  canCache
  html . renderHtml $ frontPage

pageAbout :: ActionC ()
pageAbout = do
  canCache
  html . renderHtml $ aboutPage

css t = do
  setHeader "Content-Type" "text/css; charset=utf-8"
  raw $ DLT.encodeUtf8 t

getTheme :: ActionC ()
getTheme = do
  canCache
  name <- param "name"
  css . renderHtml $ getStyleCss name

getBaseStyle :: ActionC ()
getBaseStyle = do
  canCache
  type' <- param "type"
  when (type' `elem` ["front", "paste"]) $ do
    css . renderHtml $ getPageCss type'

