{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import qualified Data.Text                            as ST
import           Data.Text.Format
import           Data.Text.Lazy                       (Text, toStrict)
import           Data.Text.Lazy                       as T

import           Text.Blaze.Html.Renderer.Text

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger

import           Network.HTTP.Types.Status

import           Database.PostgreSQL.Simple

import           Data.Char
import           Data.Pool
import           System.Random

import           Web.Scotty.Internal.Types
import           Web.Scotty.Trans

import           Control.Monad
import           Control.Monad.Reader
import           Data.Default.Class                   (def)
import           Data.Maybe
import Data.Yaml

import GHC.Generics


import           Db
import           Templates

data Config = Config { port      :: Int
                     , maxLength :: Int
                     }

instance FromJson Config where
  parseJson (Object v) = Config
    <$> v .:? "port" .!= 3000
    <*> v .:? "maxLength" .!= 5000

data AppState = AppState { config   :: Config
                         , connPool :: Pool Connection
                         }

newtype AppStateM a = AppStateM { runAppStateM :: ReaderT AppState IO a
                                } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

dbinfo = defaultConnectInfo { connectUser = "postgres"
                            , connectDatabase = "somepaste"
                            }

spawnConn :: IO Connection
spawnConn = connect dbinfo

onlyAlpha :: IO Char -> IO Char
onlyAlpha gen = gen >>= \c -> if isAlpha c then return c else onlyAlpha gen

randomHash :: IO Text
randomHash = do
  rands <- replicateM 40 . onlyAlpha $ getStdRandom (randomR ('0', 'Z'))
  return $ T.pack rands

maybeParam :: Text -> Text -> ActionT Text AppStateM Text
maybeParam d key = do
  paramList <- params
  return . fromMaybe d $ lookup key paramList

retrievePaste :: ActionT Text AppStateM ()
retrievePaste = do
  key <- param "key"
  theme <- maybeParam "plain" "theme"
  pool <- lift $ asks connPool
  paste <- liftAndCatchIO $ withResource pool (`getPaste` key)
  case paste of
    Just p ->  html . renderHtml $ viewPaste p theme
    Nothing -> do
      status status404
      html $ format "Paste {} not found" [key]

savePaste :: ActionT Text AppStateM ()
savePaste = do
  lang <- maybeParam "plain" "lang"
  text <- param "text"
  key <- liftAndCatchIO randomHash
  pool <- lift $ asks connPool
  liftAndCatchIO $ withResource pool (\conn -> insertPaste conn text key lang)
  status created201
  redirect (format "/paste/{}" [key])

retrievePasteRaw :: ActionT Text AppStateM ()
retrievePasteRaw = do
  key <- param "key"
  pool <- lift $ asks connPool
  paste <- liftAndCatchIO $ withResource pool (`getPaste` key)
  case paste of
    Just p -> text . plainPaste $ p
    Nothing -> do
      status status404
      html $ format "Paste {} not found" [key]

pageIndex :: ActionT Text AppStateM ()
pageIndex = html . renderHtml $ frontPage

app :: ScottyT Text AppStateM ()
app = do
  middleware . gzip $ def { gzipFiles = GzipCompress }
  middleware logStdoutDev

  get "/" pageIndex
  get "/paste/:key" retrievePaste
  get "/paste/raw/:key" retrievePasteRaw
  post "/paste" savePaste

main = do
  pool <- createPool spawnConn close 2 10 5
  conf <- -- TODO
  scottyOptsT def (runIO $ appState conf pool) app where
        runIO :: AppState -> AppStateM a -> IO a
        runIO c m = runReaderT (runAppStateM m) c

        appState :: Config -> Pool Connection -> AppState
        appState c p = AppState { config = c
                                , connPool = p
                                }
