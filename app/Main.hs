{-# LANGUAGE OverloadedStrings #-}

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

maybeParam :: Text -> ActionT Text IO (Maybe Text)
maybeParam key = do
  paramList <- params
  return $ lookup key paramList

retrievePaste :: Connection -> ActionM ()
retrievePaste conn = do
  key <- param "key"
  theme <- maybeParam "theme"
  paste <- liftAndCatchIO $ getPaste conn key
  case paste of
    Just p -> html . renderHtml $ viewPaste p theme
    Nothing -> do
      status status404
      html $ format "Paste {} not found" [key]

savePaste :: Connection -> ActionM ()
savePaste conn = do
  lang <- fromMaybe "plain" <$> maybeParam "lang"
  text <- param "text"
  key <- liftAndCatchIO randomHash
  liftAndCatchIO $ insertPaste conn text key lang
  status created201
  redirect (format "/paste/{}" [key])

retrievePasteRaw :: Connection -> ActionM ()
retrievePasteRaw conn = do
  key <- param "key"
  paste <- liftAndCatchIO $ getPaste conn key
  case paste of
    Just p -> text . plainPaste $ p
    Nothing -> do
      status status404
      html $ format "Paste {} not found" [key]

pageIndex :: Connection -> ActionM()
pageIndex = return . html . renderHtml $ frontPage

main = do
  pool <- createPool spawnConn close 2 10 5
  withResource pool $
    \conn ->
      scottyOpts opts $ do

      middleware . gzip $ def { gzipFiles = GzipCompress }
      middleware logStdoutDev

      get "/" $ pageIndex conn
      get "/paste/:key" $ retrievePaste conn
      get "/paste/raw/:key" $ retrievePasteRaw conn
      post "/paste" $ savePaste conn

