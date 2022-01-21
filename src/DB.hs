{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module DB where

import Control.Lens (makeClassy, makeClassyPrisms, prism, view)
import Control.Monad.Except (MonadError, MonadIO (..))
import Control.Monad.Reader (MonadIO (..), MonadReader)

data DbConfig = DbConfig
  { _dbConn :: Int
  , _schema :: String
  }
  deriving (Show)

makeClassy ''DbConfig

data DbError
  = QueryError String
  | InvalidConnection
  deriving (Show)

makeClassyPrisms ''DbError

loadFromDb
  :: ( MonadError e m
     , MonadReader r m
     , AsDbError e
     , HasDbConfig r
     , MonadIO m
     )
  => m String
loadFromDb = do
  dbconfig <- view dbConfig
  liftIO $ putStrLn ("connecting to db with this config: " ++ show dbconfig)
  return "important Data"
