{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader                 (runReaderT, unless)
import qualified Data.ByteString                      as B
import           Data.Maybe                           (fromMaybe)
import           Data.Pool                            (Pool, createPool,
                                                       withResource)
import           Data.Text.Lazy                       (Text)
import           Database.PostgreSQL.Simple           (Connection, close,
                                                       connect, connectDatabase,
                                                       connectHost,
                                                       connectPassword,
                                                       connectPort, connectUser,
                                                       defaultConnectInfo)
import           Db                                   (createTable)
import           Network.HTTP.Types.Status            (status429)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Throttle
import           Serve
import           System.Directory                     (doesFileExist)
import           System.Envy
import           System.IO
import           Web.Scotty.Internal.Types            hiding (Middleware)
import           Web.Scotty.Trans


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
  conf <- fromMaybe defConfig <$> decode
  print conf
  let dbinfo = defaultConnectInfo { connectHost = pgHost conf
                                  , connectPort = pgPort conf
                                  , connectUser = pgUser conf
                                  , connectPassword = pgPass conf
                                  , connectDatabase = pgDb conf
                                  }
  pool <- createPool (connect dbinfo) close 2 10 5
  withResource pool createTable

  st <- initThrottler
  scottyT (port conf) (runIO $ appState conf pool) (app st) where
        runIO :: AppState -> AppStateM a -> IO a
        runIO c m = runReaderT (runAppStateM m) c

        appState :: Config -> Pool Connection -> AppState
        appState c p = AppState { config = c
                                , connPool = p
                                }
