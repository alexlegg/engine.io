{-# LANGUAGE OverloadedStrings #-}
module Network.EngineIO.Snap (snapAPI) where

import Control.Applicative

import qualified Control.Monad.CatchIO as MonadCatchIO
import qualified Data.Attoparsec.Enumerator as Attoparsec
import qualified Data.ByteString.Builder as Builder
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Lazy as LMap
import qualified Network.EngineIO as EIO
import qualified Network.WebSockets.Snap as Snap
import qualified Snap.Core as Snap

--------------------------------------------------------------------------------
-- | A drop in 'EIO.ServerAPI' that works in any Snap monad - including both
-- @Handler@ and @Snap@.
snapAPI :: Snap.MonadSnap m => EIO.ServerAPI m IO
snapAPI = EIO.ServerAPI
  { EIO.srvTerminateWithResponse = \code ct body -> do
      Snap.modifyResponse $ Snap.setResponseCode code . Snap.setContentType ct
      Snap.writeLBS $ Builder.toLazyByteString body
      Snap.getResponse >>= Snap.finishWith

  , EIO.srvGetQueryParams =
      LMap.foldlWithKey' (\m k v -> HashMap.insert k v m) HashMap.empty
        <$> Snap.getQueryParams

  , EIO.srvParseRequestBody =
      fmap (either (\e@Attoparsec.ParseError{} -> Left (show e))
                   Right) .
      MonadCatchIO.try . Snap.runRequestBody . Attoparsec.iterParser

  , EIO.srvGetRequestMethod = do
      m <- Snap.getsRequest Snap.rqMethod
      return $ case m of
        Snap.GET -> "GET"
        Snap.POST -> "POST"
        Snap.HEAD -> "HEAD"
        Snap.PUT -> "PUT"
        Snap.DELETE -> "DELETE"
        Snap.TRACE -> "TRACE"
        Snap.OPTIONS -> "OPTIONS"
        Snap.CONNECT -> "CONNECT"
        Snap.PATCH -> "PATCH"
        Snap.Method method -> method

  , EIO.srvRunWebSocket = Snap.runWebSocketsSnap

  , EIO.srvSocketAppToIO = return id
  }
