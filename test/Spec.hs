{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad.Reader       ( runReaderT)
import           Data.Maybe                 (fromMaybe)
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (ConnectInfo, Connection, close,
                                             connect, connectDatabase,
                                             connectHost, connectPassword,
                                             connectPort, connectUser,
                                             defaultConnectInfo)
import qualified Db
import           Network.Wai                (Application, Response)
import           Serve                      (AppState, AppStateM, Config, pgDatabase,
                                             pgHost, pgPass, pgPort, pgUser)
import qualified Serve
import           System.Envy
import           Test.Hspec
import           Test.Hspec.Wai
import qualified Web.Scotty.Trans           as Trans
-- Specialise for our config monad

scottyAppC :: (Monad m) => Serve.Config -> Pool Connection -> (m Response -> IO Response) -> Trans.ScottyT Text Serve.AppStateM () -> IO Application
scottyAppC conf pool = return $ Trans.scottyAppT (runIO $ appState conf pool)
  where
    runIO :: AppState -> AppStateM a -> IO a
    runIO c m = runReaderT (Serve.runAppStateM m) c

    appState :: Config -> Pool Connection -> AppState
    appState c p = Serve.AppState { Serve.config = c
                                  , Serve.connPool = p
                                  }

app :: ConnectInfo -> IO Application
app dbinfo = do
  pool <- createPool (connect dbinfo) close 2 10 5
  Trans.scottyAppT (runIO $ appState defConfig pool) $ do
  -- We'll only test the raw endpoints

    Trans.get "/paste/raw/:key" Serve.retrievePasteRaw
    Trans.post "/paste" Serve.savePaste
  where
    runIO :: AppState -> AppStateM a -> IO a
    runIO c m = runReaderT (Serve.runAppStateM m) c

    appState :: Config -> Pool Connection -> AppState
    appState c p = Serve.AppState { Serve.config = c
                                  , Serve.connPool = p
                                  }


spec :: ConnectInfo -> Spec
spec dbinfo = with (app dbinfo) $ do
  describe "POST /paste" $ do
    it "responds with 302" $
      postHtmlForm "/paste" [("text", "aaaaa")] `shouldRespondWith` 302

    it "responds with 500" $
      postHtmlForm "/paste" [("text", replicate 20001 'a')] `shouldRespondWith` 500

    it "has 'Location: /paste/-138915964'" $
      postHtmlForm "/paste" [("text", "wew"), ("lang", "ABC")]
        `shouldRespondWith` 302 { matchHeaders = ["Location" <:> "/paste/-138915964"]
                                }

  describe "GET /paste/raw/-138915964" $ do
    it "responds with 200" $
      get "/paste/raw/-138915964" `shouldRespondWith` 200

    it "responds with 'wew'" $
      get "/paste/raw/-138915964" `shouldRespondWith` "wew"

main :: IO ()
main = do
  conf <- fromMaybe defConfig <$> decode
  let dbinfo = defaultConnectInfo { connectHost = pgHost conf
                                  , connectPort = pgPort conf
                                  , connectUser = pgUser conf
                                  , connectPassword = pgPass conf
                                  , connectDatabase = pgDatabase conf
                                  }
  conn <- connect dbinfo
  Db.dropTable conn -- cleanup
  Db.createTable conn
  hspec $ spec dbinfo
