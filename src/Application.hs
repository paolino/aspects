{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Application where

import App (App (unApp), AppConfig (..), AppError (AppNetError))
import Control.Lens ()
import Control.Monad.Except
  ( MonadError (throwError)
  , MonadIO
  , runExceptT
  )
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import DB
  ( AsDbError
  , DbConfig (DbConfig, _dbConn, _schema)
  , HasDbConfig
  , loadFromDb
  )
import Network
  ( AsNetworkError
  , HasNetworkConfig
  , NetworkConfig (NetConfig, _port, _ssl)
  , NetworkError (NoConnection)
  , sendOverNet
  )

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { _appDbConfig = DbConfig {_dbConn = 404, _schema = "ok"}
    , _appNetConfig = NetConfig {_port = 102, _ssl = "secure"}
    }

appMain :: App ()
appMain = do
  loadFromDb >>= sendOverNet

reportResult :: Either AppError () -> IO ()
reportResult (Right ()) = putStrLn "success"
reportResult (Left e) = putStrLn $ "error: " <> show e

main :: IO ()
main = do
  res <- runExceptT $ runReaderT (unApp appMain) defaultAppConfig
  reportResult res
