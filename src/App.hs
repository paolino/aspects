{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module App where

import Control.Lens (lens, makeClassy, prism)
import Control.Monad.Except (ExceptT, MonadError, MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT))
import DB
  ( AsDbError (_DbError)
  , DbConfig
  , DbError
  , HasDbConfig (dbConfig)
  )
import Network
  ( AsNetworkError (_NetworkError)
  , HasNetworkConfig (networkConfig)
  , NetworkConfig
  , NetworkError
  )

data AppConfig = AppConfig
  { _appDbConfig :: DbConfig
  , _appNetConfig :: NetworkConfig
  }
  deriving (Show)

makeClassy ''AppConfig

instance HasDbConfig AppConfig where
  dbConfig =
    lens
      _appDbConfig
      (\app db -> app {_appDbConfig = db})

instance HasNetworkConfig AppConfig where
  networkConfig =
    lens
      _appNetConfig
      (\app net -> app {_appNetConfig = net})

data AppError
  = AppDbError {dbError :: DbError}
  | AppNetError {netError :: NetworkError}
  deriving (Show)

instance AsDbError AppError where
  _DbError =
    prism AppDbError $ \case
      (AppDbError dbe) -> Right dbe
      appError -> Left appError

instance AsNetworkError AppError where
  _NetworkError =
    prism AppNetError $ \case
      (AppNetError nte) -> Right nte
      appError -> Left appError

newtype App a = App
  { unApp :: ReaderT AppConfig (ExceptT AppError IO) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader AppConfig
    , MonadError AppError
    , MonadIO
    )
