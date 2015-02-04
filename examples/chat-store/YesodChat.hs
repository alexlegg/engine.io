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
module YesodChat (
      YesodChat(..)
    , Message(..)
    , migrateAll
    ) where

import qualified Yesod.Core as YC
import Yesod.Persist
import Database.Persist.Sqlite
import qualified Data.Text as Text
import Paths_chat_store (getDataDir)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Message
    user Text.Text
    message Text.Text
    deriving Show
|]

YC.mkYesod "YesodChat" [YC.parseRoutesNoCheck|
/ IndexR GET
/main.js MainJSR GET
/style.css StyleCSSR GET
/socket.io/ SocketIOR
|]

data YesodChat = YesodChat {
      socketIoHandler   :: YC.HandlerT YesodChat IO ()
    , persistConfig     :: SqliteConf
    , connPool          :: PersistConfigPool SqliteConf
    }

instance YesodPersist YesodChat where
  type YesodPersistBackend YesodChat = SqlBackend
  runDB = defaultRunDB persistConfig connPool

instance YC.Yesod YesodChat where
  -- do not redirect /socket.io/?bla=blub to /socket.io?bla=blub
  cleanPath _ ["socket.io",""] = Right ["socket.io"]
  cleanPath _ p = Right p

getIndexR :: Handler ()
getIndexR = do
  dataDir <- YC.liftIO getDataDir
  YC.sendFile "text/html" $ dataDir ++ "/index.html"

getStyleCSSR :: Handler ()
getStyleCSSR = do
  dataDir <- YC.liftIO getDataDir
  YC.sendFile "text/css" $ dataDir ++ "/style.css"

getMainJSR :: Handler ()
getMainJSR = do
  dataDir <- YC.liftIO getDataDir
  YC.sendFile "application/javascript" $ dataDir ++ "/main.js"

handleSocketIOR :: Handler ()
handleSocketIOR = YC.getYesod >>= socketIoHandler
