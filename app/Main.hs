{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import qualified Data.Text                            as ST
import           Data.Text.Encoding
import           Data.Text.Format
import           Data.Text.Lazy                       (Text, toStrict)
import qualified Data.Text.Lazy                       as T

import           Text.Blaze.Html.Renderer.Text

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Throttle

import           Network.HTTP.Types.Status

import           Database.PostgreSQL.Simple

import           Data.Char
import           Data.Pool

import           Web.Scotty.Internal.Types            hiding (Middleware)
import           Web.Scotty.Trans

import           Control.Monad
import           Control.Monad.Reader
import           Data.Default.Class                   (def)
import           Data.Maybe

import           System.Directory                     (doesFileExist)

import qualified Data.ByteString                      as B

import           Data.Yaml
import           Data.Yaml.Config

import           Data.Int                             (Int32, Int64)

import           Data.Digest.XXHash                   (xxHash')

import           System.IO

import           Db
import           Templates

defaultConfig = Config 3000 20000 "localhost" "postgres" "somepaste"

data Config = Config { port         :: Int
                     , maxLength    :: Int64
                     , appUrl       :: Text
                     , postgresUser :: String
                     , postgresDb   :: String
                     }

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .:? "port" .!= 3000
    <*> v .:? "max-length" .!= 5000
    <*> v .:? "app-url" .!= "localhost"
    <*> v .: "postgres-user"
    <*> v .: "postgres-db"

instance ToJSON Config where
  toJSON (Config p l u ps db) = object [ "port" .= p
                                       , "max-length" .= l
                                       , "app-url" .= u
                                       , "postgres-user" .= ps
                                       , "postgres-db" .= db
                                       ]

data AppState = AppState { config   :: Config
                         , connPool :: Pool Connection
                         }

newtype AppStateM a = AppStateM { runAppStateM :: ReaderT AppState IO a
                                } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

type ActionC = ActionT Text AppStateM

hashPaste :: ST.Text -> Int32
hashPaste = fromIntegral . xxHash' . encodeUtf8

makeDefaultConfig :: FilePath -> IO ()
makeDefaultConfig configFile = do
  exists <- doesFileExist configFile
  unless exists $ B.writeFile configFile $ encode defaultConfig

spawnConn :: ConnectInfo -> IO Connection
spawnConn = connect

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
  paste <- liftAndCatchIO $ withResource pool (`getPaste` key)
  case paste of
    Just p  ->  html . renderHtml $ viewPaste p theme
    Nothing -> errorWith status404 $ format "Paste {} not found" [key]

savePaste :: ActionC ()
savePaste = do
  maxLength <- lift (maxLength <$> asks config)
  lang <- maybeParam "plain" "lang"
  paste <- param "text"
  when (T.length paste > maxLength) $ raise (format "Paste over length: {}" [maxLength])
  let key = hashPaste $ T.toStrict (T.append paste lang)
  pool <- lift $ asks connPool
  liftAndCatchIO $ withResource pool (\conn -> insertPaste conn paste key lang)
  redirect (format "/paste/{}" [key])

retrievePasteRaw :: ActionC ()
retrievePasteRaw = do
  key <- param "key"
  pool <- lift $ asks connPool
  paste <- liftAndCatchIO $ withResource pool (`getPaste` key)
  case paste of
    Just p  -> text . plainPaste $ p
    Nothing -> errorWith status404 $ format "Paste {} not found" [key]

pageIndex :: ActionC ()
pageIndex = html . renderHtml $ frontPage

app :: WaiThrottle -> ScottyT Text AppStateM ()
app throttler = do
  let settings = defaultThrottleSettings { onThrottled    = onThrottled'
                                         , throttleBurst  = 10
                                         , throttlePeriod = 10^7
                                         , throttleRate   = 6
                                         }

  middleware . gzip $ def { gzipFiles = GzipCompress }
  middleware logStdoutDev
  middleware $ throttle settings throttler

  get "/" pageIndex
  get "/paste/:key" retrievePaste
  get "/paste/raw/:key" retrievePasteRaw
  post "/paste" savePaste

onThrottled' _ = responseLBS status429
                 [("Content-Type", "text/plain; charset=utf-8")]
                 "You have been ratelimited... Stop trying to paste so much!"

main = do
  makeDefaultConfig "config.yaml"
  conf <- loadYamlSettings ["config.yaml"] [] useEnv
  let dbinfo = defaultConnectInfo { connectUser = postgresUser conf
                                  , connectDatabase = postgresDb conf
                                  }
  pool <- createPool (spawnConn dbinfo) close 2 10 5
  withResource pool createTable

  st <- initThrottler
  scottyOptsT def (runIO $ appState conf pool) (app st) where
        runIO :: AppState -> AppStateM a -> IO a
        runIO c m = runReaderT (runAppStateM m) c

        appState :: Config -> Pool Connection -> AppState
        appState c p = AppState { config = c
                                , connPool = p
                                }
