{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Main where

import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Text.Format
import           Data.Text.Lazy                       (toStrict)

import           Text.Blaze.Html.Renderer.Text

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger

import           Network.HTTP.Types.Status

import           Database.PostgreSQL.Simple

import           Data.Pool
import           System.Random
import Data.Char

import           Web.Scotty
import           Web.Scotty.Internal.Types

import           Control.Monad
import           Data.Maybe

import           Db
import           Templates

opts = Options { verbose = 1
               , settings = defaultSettings
               }

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

--maybeParam :: Text -> ActionM (Maybe Text)
maybeParam params key = do -- TODO: make this be in the scotty monad
  --paramList <- params
  toStrict <$> lookup key params

--retrievePaste :: ScottyT Text IO ()
retrievePaste conn = do
  key <- param "key"
  prms <- params
  let theme = maybeParam prms "theme"
  paste <- liftAndCatchIO $ getPaste conn key
  case paste of
    Just p -> html . renderHtml $ viewPaste p theme
    Nothing -> do
      status status404
      html (format "Paste {} not found" [key])

--savePaste :: ScottyT Text IO ()
savePaste conn = do
  prms <- params
  let lang = fromMaybe "plain" $ maybeParam prms "lang"
  text <- param "text"
  key <- liftAndCatchIO randomHash
  liftAndCatchIO $ insertPaste conn text key lang
  status created201
  redirect (format "/paste/{}" [key])

main = do
  pool <- createPool spawnConn close 2 10 5
  withResource pool $
    \conn ->
      scottyOpts opts $ do

      middleware . gzip $ def { gzipFiles = GzipCompress }
      middleware logStdoutDev

      get "/paste/:key" $ retrievePaste conn
      post "/paste" $ savePaste conn
