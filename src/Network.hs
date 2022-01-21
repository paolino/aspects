{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Network where

import Control.Lens (makeClassy, makeClassyPrisms, view, (#))
import Control.Monad.Except (MonadError (throwError), MonadIO (..))
import Control.Monad.Reader (MonadReader)

data NetworkConfig = NetConfig
  { _port :: Int
  , _ssl :: String
  }
  deriving (Show)

makeClassy ''NetworkConfig

data NetworkError
  = Timeout Int
  | NoConnection
  deriving (Show)

makeClassyPrisms ''NetworkError

sendOverNet
  :: ( MonadError e m
     , MonadReader r m
     , AsNetworkError e
     , HasNetworkConfig r
     , MonadIO m
     )
  => String
  -> m ()
sendOverNet s = do
  netConfig <- view networkConfig
  liftIO $ putStrLn ("data to send: " ++ s)
  liftIO $ putStrLn ("connecting to the network with this config: " ++ show netConfig)
  throwError $ _NetworkError # NoConnection
