{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.JsonRpcCliHttp
  ( HTTP.Request(..)
  , httpRequest
  , HTTP.parseRequest_
  , runJsonRpcHttpT
  ) where

import Control.Exception (Exception(..), try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Data.Aeson.JsonRpc
import Data.Aeson.Types
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Either (either)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4
import Network.Connection (TLSSettings(..))
import Network.Constants
import Network.HTTP.Client (httpLbs,newManager)
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import Network.JsonRpcConn

instance JsonRpcConn HTTP.Request where
  sendJsonRpc' :: ( MonadLoggerIO m, MonadBaseControl IO m
                  , ToJSON r, ToRequest r, FromJSON a)
               => JsonRpcVersion -> r -> JsonRpcConnT HTTP.Request m (JsonRpcResp a)
  sendJsonRpc' ver rq = do
    logDebug' rq
    let reqmn = requestMethod rq
    let reqmps = toJSON rq
    uuid_ <- liftIO nextRandom
    let reqm = JsonRpcRequest ver reqmn reqmps $ JsonRpcIdTxt $ toText uuid_
    defReq <- ask
    let req = HTTP.setRequestMethod "POST"
            $ HTTP.setRequestHeader HTTP.hContentType [C8.pack $ snd hContentTypeValue]
            $ HTTP.setRequestHeader HTTP.hUserAgent [C8.pack $ snd hUserAgentValue]
            $ HTTP.setRequestBodyLBS (jsonRpcEncodeRequest reqm) defReq
--    logDebugN ("Request: " <> T.pack (show req))
    logDebugN ("Body: " <> T.pack (LC8.unpack $ jsonRpcEncodeRequest reqm))
    let mgrs = TLS.mkManagerSettings (TLSSettingsSimple True True False) Nothing
    eresp <- liftIO (newManager mgrs >>= try . httpLbs req)
    logDebugN (T.pack $ either displayException (LC8.unpack . HTTP.getResponseBody) eresp)
    --logDebugN (T.pack $ LC8.unpack $ HTTP.getResponseBody eresp)
    return $ case eresp of
                Left ex -> Left $ T.pack $ displayException (ex :: HTTP.HttpException)
                Right resp -> jsonRpcDecodeResponseResult
                            $ jsonRpcDecodeResponse
                            $ HTTP.getResponseBody resp
    where
        displayException' = const "httpLbs exception"

httpRequest :: ByteString         -- ^ Host
            -> Maybe Int          -- ^ Puerto
            -> Maybe ByteString   -- ^ Path
            -> HTTP.Request
httpRequest h mpo mpa = HTTP.setRequestHost h
                      $ maybeReq HTTP.setRequestPort mpo
                      $ maybeReq HTTP.setRequestPath mpa HTTP.defaultRequest
  where
    maybeReq f md rq = case md of
      Nothing -> rq
      Just d -> f d rq

-- | Ejecuta sesión JSON-RPC sobre HTTP
runJsonRpcHttpT :: (MonadLoggerIO m, MonadBaseControl IO m, FromJSON a)
                => HTTP.Request -> JsonRpcConnT HTTP.Request m a -> m (Either Text a)
runJsonRpcHttpT = runJsonRpcConnT

