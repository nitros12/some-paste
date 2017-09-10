{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text                            (Text)
import           Data.Text.Format
import           Data.Text.Lazy                       (toStrict)

import           Text.Blaze.Html.Renderer.Text

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger

import           Network.HTTP.Types.Status

import           Database.PostgreSQL.Simple

import           Web.Scotty

import           Db
import           Templates

main = do
  conn <- connectPostgreSQL "user='postgres' dbname='somepaste'"
  let opts = Options { verbose = 1
                     , settings = defaultSettings
                     }
  scottyOpts opts $ do

    middleware . gzip $ def { gzipFiles = GzipCompress }
    middleware logStdoutDev

    get "/paste/:key" $ do
      key <- param "key"
      prms <- params
      paste <- liftAndCatchIO $ getPaste conn key
      case paste of
        Just p -> html . renderHtml $ viewPaste p (toStrict <$> lookup "theme" prms)
        Nothing -> do
          status status404
          html (format "Paste {} not found" [key])
