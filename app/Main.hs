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

--import           Web.Scotty
import           Web.Scotty.Internal.Types
import  Web.Scotty.Trans

import           Control.Monad
import           Control.Monad.Reader
import           Data.Default.Class                   (def)
import           Data.Maybe

import qualified Data.ConfigFile                      as Conf

import           Db
import           Templates

data Config = Config { port      :: Int
                     , maxLength :: Int
                     , connPool :: Pool Connection
                     }

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a
                            } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

--type Trans.ScottyT Text ConfigM = Trans.ScottyT Text ConfigM

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

maybeParam :: Text -> Text -> ActionT Text ConfigM Text
maybeParam d key = do
  paramList <- params
  return . fromMaybe d $ lookup key paramList

retrievePaste :: ActionT Text ConfigM ()
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

savePaste :: ActionT Text ConfigM ()
savePaste = do
  lang <- maybeParam "plain" "lang"
  text <- param "text"
  key <- liftAndCatchIO randomHash
  pool <- lift $ asks connPool
  liftAndCatchIO $ withResource pool (\conn -> insertPaste conn text key lang)
  status created201
  redirect (format "/paste/{}" [key])

retrievePasteRaw :: ActionT Text ConfigM ()
retrievePasteRaw = do
  key <- param "key"
  pool <- lift $ asks connPool
  paste <- liftAndCatchIO $ withResource pool (`getPaste` key)
  case paste of
    Just p -> text . plainPaste $ p
    Nothing -> do
      status status404
      html $ format "Paste {} not found" [key]

pageIndex :: ActionT Text ConfigM ()
pageIndex = html . renderHtml $ frontPage

app :: ScottyT Text ConfigM ()
app = do
  middleware . gzip $ def { gzipFiles = GzipCompress }
  middleware logStdoutDev

  get "/" pageIndex
  get "/paste/:key" retrievePaste
  --get "/paste/raw/:key" $ retrievePasteRaw
  post "/paste" savePaste

main = do
  pool <- createPool spawnConn close 2 10 5
  scottyOptsT def (runIO $ config pool) app where
        runIO :: Config -> ConfigM a -> IO a
        runIO c m = runReaderT (runConfigM m) c

        config :: Pool Connection -> Config
        config c = Config { port = 3000
                          , maxLength = 5000
                          , connPool = c
                          }
