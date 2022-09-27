{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module OTel.Instrumentation.Wai.Internal
  ( -- * Disclaimer
    -- $disclaimer
    buildMiddleware
  , middleware
  , spanSpecFromRequest
  , attrsFromRequest
  , includeReqLengthIfKnown
  , decodeBytes
  ) where

import Control.Monad (guard)
import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import Data.ByteString (ByteString)
import Data.Foldable (Foldable(..))
import Data.IP (fromHostAddress, fromHostAddress6)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word64)
import Network.HTTP.Types (Status(..))
import Network.Socket (SockAddr(..), PortNumber)
import Network.Wai
  ( Request
    ( httpVersion, rawPathInfo, rawQueryString, remoteHost, requestBodyLength, requestHeaderReferer
    , requestHeaderUserAgent, requestMethod
    )
  , RequestBodyLength(..), Middleware, requestHeaderHost, responseStatus
  )
import OTel.API.Common (AttrsFor(..), (.@), AttrsBuilder, Key)
import OTel.API.Trace (SpanName(..), SpanSpec, TracerProvider, TracingBackend)
import Prelude
import qualified Data.ByteString as ByteString
import qualified OTel.API.Common as OTel
import qualified OTel.API.Trace as OTel

buildMiddleware :: TracerProvider -> IO Middleware
buildMiddleware tracerProvider = do
  tracingBackend <- OTel.getTracingBackend tracerProvider "otel-instrumentation-wai"
    { OTel.instrumentationScopeVersion = Just "0.0.0" -- TODO: Automatically pull package version
    , OTel.instrumentationScopeSchemaURL = Just OTel.TRACE_SCHEMA_URL
    }
  pure $ middleware tracingBackend

middleware :: TracingBackend -> Middleware
middleware tracingBackend app req sendResp = do
  flip OTel.runTracingT tracingBackend do
    -- TODO: Need to check if propagation is needed from request when propagator support is added
    OTel.trace (spanSpecFromRequest req) \mutableSpan -> do
      withRunInIO \runInIO -> do
        app req \resp -> do
          let Status { statusCode, statusMessage } = responseStatus resp
          runInIO $ OTel.updateSpan mutableSpan OTel.defaultUpdateSpanSpec
            { OTel.updateSpanSpecAttrs =
                Just $ OTel.HTTP_STATUS_CODE .@ statusCode
            , OTel.updateSpanSpecStatus = do
                guard $ statusCode >= 500
                pure $ OTel.SpanStatusError $ decodeBytes statusMessage
            }
          sendResp resp

spanSpecFromRequest :: Request -> SpanSpec
spanSpecFromRequest req =
  OTel.defaultSpanSpec
    { OTel.spanSpecName = SpanName $ decodeBytes $ rawPathInfo req
    , OTel.spanSpecKind = OTel.SpanKindServer
    , OTel.spanSpecAttrs = attrsFromRequest req
    }

attrsFromRequest :: Request -> AttrsBuilder 'AttrsForSpan
attrsFromRequest req =
  fold $ catMaybes
    [ Just $ OTel.HTTP_METHOD .@ decodeBytes (requestMethod req)
    , Just $ OTel.HTTP_FLAVOR .@ show (httpVersion req)
    , case remoteHost req of
        SockAddrUnix _path ->
          Nothing
        SockAddrInet port hostAddress ->
          Just
            $ OTel.NET_HOST_IP .@ show (fromHostAddress hostAddress)
                <> OTel.NET_HOST_PORT .@ fromIntegral @PortNumber @Int64  port
        SockAddrInet6 port _flowInfo hostAddress6 _scopeID ->
          Just
            $ OTel.NET_HOST_IP .@ show (fromHostAddress6 hostAddress6)
                <> OTel.NET_HOST_PORT .@ fromIntegral @PortNumber @Int64  port
    , includeIfNotNull req OTel.HTTP_HOST requestHeaderHost
    , includeIfNotNull req OTel.HTTP_USER_AGENT requestHeaderUserAgent
    , includeIfNotNull req OTel.HTTP_TARGET \r ->
        Just $ rawPathInfo r <> rawQueryString r
    , includeIfNotNull req OTel.NET_PEER_NAME requestHeaderReferer
    , includeReqLengthIfKnown req
    ]

includeIfNotNull
  :: Request
  -> Key Text
  -> (Request -> Maybe ByteString)
  -> Maybe (AttrsBuilder 'AttrsForSpan)
includeIfNotNull req key selector = do
  val <- selector req
  guard $ not $ ByteString.null val
  pure $ key .@ decodeBytes val

includeReqLengthIfKnown :: Request -> Maybe (AttrsBuilder 'AttrsForSpan)
includeReqLengthIfKnown req = do
  KnownLength len <- pure $ requestBodyLength req
  guard $ len > 0 && len <= fromIntegral (maxBound @Int64)
  pure $ OTel.HTTP_REQUEST_CONTENT_LENGTH .@ fromIntegral @Word64 @Int64 len

decodeBytes :: ByteString -> Text
decodeBytes = decodeUtf8With lenientDecode

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with utmost care,
-- otherwise invariants will easily be violated.
