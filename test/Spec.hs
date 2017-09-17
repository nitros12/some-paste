{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad.Reader                 (runReaderT)
import           Data.Pool
import           Data.Text                            (Text)
import           Database.PostgreSQL.Simple           (Connection, close,
                                                       connect, connectDatabase,
                                                       connectUser,
                                                       defaultConnectInfo)
import qualified Db                                   as Db
import           Network.Wai                          (Application, Response)
import           Network.Wai.Middleware.RequestLogger
import           Serve                                (AppState, AppStateM,
                                                       Config)
import qualified Serve                                as Serve
import           System.IO
import           Test.Hspec
import           Test.Hspec.Wai
import qualified Web.Scotty                           as S
import           Web.Scotty.Internal.Types            hiding (Application,
                                                       Middleware)
import qualified Web.Scotty.Trans                     as Trans
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

dbinfo = defaultConnectInfo { connectUser = "postgres"
                            , connectDatabase = "pastetest"
                            }


app :: IO Application
app = do

  pool <- createPool (connect dbinfo) close 2 10 5
  Trans.scottyAppT (runIO $ appState Serve.defaultConfig pool) $ do
  -- We'll only test the raw endpoints
    Trans.middleware logStdoutDev
    Trans.get "/paste/raw/:key" Serve.retrievePasteRaw
    Trans.post "/paste" Serve.savePaste
  where
    runIO :: AppState -> AppStateM a -> IO a
    runIO c m = runReaderT (Serve.runAppStateM m) c

    appState :: Config -> Pool Connection -> AppState
    appState c p = Serve.AppState { Serve.config = c
                                  , Serve.connPool = p
                                  }


spec :: Spec
spec = with app $ do
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
  conn <- connect dbinfo
  Db.dropTable conn -- cleanup
  Db.createTable conn
  hspec spec
