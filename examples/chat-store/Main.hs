{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
module Main where

import Chat (server, ServerState (..))
import YesodChat

import Control.Applicative
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Yesod.Core as YC
import qualified Control.Concurrent.STM as STM
import qualified Network.SocketIO as SocketIO
import qualified Network.EngineIO.Yesod as EIOYesod
import Yesod.Persist
import Database.Persist.Sqlite

migrateAndClean = do
  runMigration migrateAll
  deleteWhere ([] :: [Filter Message])

main :: IO ()
main = do
  state <- ServerState <$> STM.newTVarIO 0
  sockets <- SocketIO.initialize EIOYesod.yesodAPI (server state)

  let dbconf = SqliteConf { sqlDatabase = ":memory:", sqlPoolSize = 10 }
  pool <- YC.liftIO $ createPoolConfig dbconf
  
  runStdoutLoggingT $ runResourceT $ runPool dbconf migrateAndClean pool

  let app = YesodChat sockets dbconf pool 
  YC.warp 8000 app
