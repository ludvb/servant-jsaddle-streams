{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Client.JSaddle
  ( client,
    ClientM,
    runClientM,
    runClientM',
    withClientM,
    withClientM',
    ClientEnv (..),
    mkClientEnv,
    getDefaultBaseUrl,
    module Servant.Client.Core.Reexport,
  )
where

import Control.Exception
  ( Exception,
  )
import Control.Lens ((^.))
import Control.Monad (forM, forM_, guard, unless)
import Control.Monad.Catch
  ( MonadCatch,
    MonadThrow,
  )
import Control.Monad.Codensity
  ( Codensity
      ( Codensity
      ),
  )
import Control.Monad.Except
  ( MonadError (catchError, throwError),
  )
import Control.Monad.IO.Class
  ( MonadIO,
  )
import Control.Monad.RWS (MonadIO (liftIO))
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
    asks,
    runReaderT,
  )
import Control.Monad.Trans
  ( MonadTrans
      ( lift
      ),
  )
import Control.Monad.Trans.Except
  ( ExceptT,
    runExceptT,
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.ByteString.Builder
  ( toLazyByteString,
  )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L
import Data.CaseInsensitive
  ( CI,
    mk,
    original,
  )
import Data.Foldable
  ( toList,
  )
import Data.Functor.Alt
  ( Alt
      ( (<!>)
      ),
  )
import qualified Data.JSString as JS
import Data.Proxy
  ( Proxy (Proxy),
  )
import qualified Data.Sequence as Seq
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import GHC.Generics
  ( Generic,
  )
import qualified GHCJS.Buffer as Buffer
import qualified GHCJS.DOM
import qualified GHCJS.DOM.Location as Location
import qualified GHCJS.DOM.ReadableStreamDefaultReader as RSDR
import qualified GHCJS.DOM.Response as Response
import GHCJS.DOM.Types
  ( DOM,
  )
import qualified GHCJS.DOM.Types as JS
import qualified GHCJS.DOM.Window as Window
import qualified JavaScript.Object as Object
import Language.Javascript.JSaddle
  ( Object (Object),
    js0,
    jsg,
    new,
    runJSaddle,
    (!),
    (!!),
  )
import Language.Javascript.JSaddle.Object (MakeObject (makeObject))
import qualified Language.Javascript.JSaddle.Types as JSaddle
import Language.Javascript.JSaddle.Value
  ( JSValue
      ( ValObject
      ),
  )
import Network.HTTP.Media
  ( MediaType,
  )
import Network.HTTP.Types
  ( Status
      ( statusCode
      ),
    http11,
    mkStatus,
    renderQuery,
  )
import Servant.Client.Core
  ( Request,
    RequestBody
      ( RequestBodyBS,
        RequestBodyLBS,
        RequestBodySource
      ),
    RequestF
      ( requestBody,
        requestHeaders,
        requestPath,
        requestQueryString
      ),
    RunClient
      ( runRequestAcceptStatus,
        throwClientError
      ),
    RunStreamingClient
      ( withStreamingRequest
      ),
    clientIn,
  )
import Servant.Client.Core.Reexport
  ( BaseUrl (..),
    ClientError (..),
    EmptyClient (..),
    HasClient (..),
    InvalidBaseUrlException,
    Response,
    ResponseF (..),
    Scheme (..),
    StreamingResponse,
    foldMapUnion,
    matchUnion,
    parseBaseUrl,
    showBaseUrl,
  )
import Servant.Client.Core.Request
  ( RequestF
      ( requestMethod
      ),
  )
import Servant.Types.SourceT
  ( runSourceT,
  )
import qualified Servant.Types.SourceT as S
import Prelude hiding ((!!))

newtype ClientEnv = ClientEnv {baseUrl :: BaseUrl}

data JSaddleConnectionError = JSaddleConnectionError
  deriving (Eq, Show)

instance Exception JSaddleConnectionError

mkClientEnv :: BaseUrl -> ClientEnv
mkClientEnv = ClientEnv

instance Show ClientEnv where
  showsPrec prec (ClientEnv burl) =
    showParen
      (prec >= 11)
      ( showString "ClientEnv {"
          . showString "baseUrl = "
          . shows burl
          . showString "}"
      )

client :: HasClient ClientM api => Proxy api -> Client ClientM api
client api = api `clientIn` (Proxy :: Proxy ClientM)

newtype ClientM a = ClientM
  {fromClientM :: ReaderT ClientEnv (ExceptT ClientError (Codensity DOM)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      Generic,
      MonadReader ClientEnv,
      MonadError ClientError
    )

deriving instance MonadThrow (Codensity DOM) => MonadThrow ClientM

deriving instance MonadCatch (Codensity DOM) => MonadCatch ClientM

newtype ByteString = ByteString {unByteString :: BS.ByteString}

instance JS.ToJSVal ByteString where
  toJSVal x = do
    (buf, _offset, _len) <- JSaddle.ghcjsPure $ Buffer.fromByteString . unByteString $ x
    mutableBuf <- JSaddle.ghcjsPure . Buffer.getArrayBuffer =<< Buffer.thaw buf
    pure (JS.pToJSVal mutableBuf)

instance JS.FromJSVal ByteString where
  fromJSVal x = do
    buf <-
      Buffer.freeze
        =<< JSaddle.ghcjsPure (Buffer.createFromArrayBuffer (JS.pFromJSVal x))
    bs <- JSaddle.ghcjsPure $ Buffer.toByteString 0 Nothing buf
    pure . Just . ByteString $ bs

instance Alt ClientM where
  a <!> b = a `catchError` const b

instance RunClient ClientM where
  throwClientError = throwError
  runRequestAcceptStatus = performRequest

instance RunStreamingClient ClientM where
  withStreamingRequest req k = performWithStreamingRequest req k

runClientM :: ClientM a -> ClientEnv -> DOM (Either ClientError a)
runClientM cm env = withClientM cm env pure

runClientM' :: ClientM a -> DOM (Either ClientError a)
runClientM' cm = withClientM' cm pure

withClientM :: ClientM a -> ClientEnv -> (Either ClientError a -> DOM b) -> DOM b
withClientM cm env k =
  let Codensity f = runExceptT $ flip runReaderT env $ fromClientM cm
   in f k

withClientM' :: ClientM a -> (Either ClientError a -> DOM b) -> DOM b
withClientM' cm k = do
  burl <- getDefaultBaseUrl
  withClientM cm (mkClientEnv burl) k

getDefaultBaseUrl :: DOM BaseUrl
getDefaultBaseUrl = do
  win <-
    GHCJS.DOM.currentWindow >>= \case
      Just x -> pure x
      Nothing -> fail "Cannot determine default base url without window."
  curLoc <- Window.getLocation win

  protocolStr <- Location.getProtocol curLoc
  portStr <- Location.getPort curLoc
  hostname <- Location.getHostname curLoc

  let protocol
        | (protocolStr :: JS.JSString) == "https:" = Https
        | otherwise = Http

      port :: Int
      port
        | null portStr = case protocol of
          Http -> 80
          Https -> 443
        | otherwise = read portStr

  pure (BaseUrl protocol hostname port "")

toUrl :: BaseUrl -> Request -> JS.JSString
toUrl burl request =
  let pathS =
        JS.toJSString
          . T.decodeUtf8With T.lenientDecode
          . L.toStrict
          . toLazyByteString
          $ requestPath request
      queryS =
        JS.toJSString
          . T.decodeUtf8With T.lenientDecode
          . renderQuery True
          . toList
          $ requestQueryString request
   in JS.toJSString (showBaseUrl burl) <> pathS <> queryS

toBody :: Request -> Maybe (IO (L.ByteString, MediaType))
toBody request = case requestBody request of
  Nothing -> Nothing
  Just (RequestBodyLBS x, mt) -> Just . pure $ (x, mt)
  Just (RequestBodyBS x, mt) -> Just . pure $ (L.fromStrict x, mt)
  Just (RequestBodySource x, mt) -> Just $ do
    res <- runExceptT . runSourceT $ x
    case res of
      Left err -> liftIO $ fail err
      Right val -> pure (mconcat val, mt)

fetch :: BaseUrl -> Request -> DOM JS.Response
fetch burl req = do
  obj <- Object.create
  headersProp <- Object.create

  forM_ (requestHeaders req) $ \(name, val) -> do
    jsval <- JS.toJSVal . ByteString $ val
    Object.setProp (JS.pack . BS.unpack . original $ name) jsval headersProp

  case toBody req of
    Nothing -> pure ()
    Just getBody -> do
      (body, mt) <- liftIO getBody

      bodyjs <- JS.toJSVal . ByteString . L.toStrict $ body
      Object.setProp "body" bodyjs obj

      mtjs <- JS.toJSVal . JS.pack . show $ mt
      Object.setProp ("Content-Type" :: JS.JSString) mtjs headersProp

  method <- JS.toJSVal . JS.pack . BS.unpack $ requestMethod req
  Object.setProp "method" method obj

  headers <- new (jsg ("Headers" :: JS.JSString)) (JS.toJSVal headersProp)
  Object.setProp "headers" headers obj

  requestInit <- JS.RequestInit <$> JS.toJSVal (ValObject obj)
  window <- jsg ("window" :: JS.JSString)
  Window.fetch (Window.Window window) (toUrl burl req) (Just requestInit)

parseHeaders :: JS.Response -> DOM [(CI BS.ByteString, BS.ByteString)]
parseHeaders resp =
  Response.getHeaders resp >>= \headers -> do
    headerEntries <- entries headers
    forM headerEntries $ \(k, v) ->
      pure
        ( mk . BS.pack . JS.unpack $ k,
          BS.pack . JS.unpack $ v
        )
  where
    entries headers = do
      headersObj <- makeObject . JS.unHeaders $ headers
      entriesObj <- headersObj ^. js0 ("entries" :: JS.JSString)
      getEntries entriesObj
    getEntries entriesObj = do
      maybeEntry <- runMaybeT $ do
        res <- lift $ entriesObj ^. js0 ("next" :: JS.JSString)
        isDone <-
          MaybeT . pure
            =<< lift (res ! ("done" :: JS.JSString) >>= JS.fromJSVal)
        guard (not isDone)
        value <- lift (res ! ("value" :: JS.JSString))
        k <- MaybeT . pure =<< lift (value !! 0 >>= JS.fromJSVal)
        v <- MaybeT . pure =<< lift (value !! 1 >>= JS.fromJSVal)
        pure (k, v)
      case maybeEntry of
        Nothing -> pure []
        Just x -> do
          xs <- getEntries entriesObj
          pure (x : xs)

createReader :: JS.Response -> DOM (S.SourceT IO BS.ByteString)
createReader resp = do
  ctx <- JS.askJSM
  pure $ S.SourceT (stepper ctx)
  where
    stepper :: JS.JSContextRef -> (S.StepT IO BS.ByteString -> IO a) -> IO a
    stepper ctx runSteps = do
      body <- runJSaddle ctx $ Response.getBody resp
      case body of
        Nothing -> runSteps $ S.Error "Failed to parse response"
        Just readableStream -> do
          reader <-
            runJSaddle ctx $
              RSDR.newReadableStreamDefaultReader
                readableStream
          steps <- getSteps ctx reader
          runSteps steps

    getSteps ::
      JS.JSContextRef ->
      RSDR.ReadableStreamDefaultReader ->
      IO (S.StepT IO BS.ByteString)
    getSteps ctx reader = do
      chunk <- runJSaddle ctx $ fmap Object (RSDR.read reader)
      maybeIsDone <- runJSaddle ctx $ Object.getProp "done" chunk >>= JS.fromJSVal
      case maybeIsDone of
        Nothing -> pure $ S.Error "Failed to parse response"
        Just isDone -> do
          if isDone
            then pure S.Stop
            else do
              value <- runJSaddle ctx $ Object.getProp "value" chunk
              Just buffer <-
                runJSaddle ctx $
                  Object.getProp "buffer" (Object value)
                    >>= JS.fromJSVal
              pure $ S.Yield (unByteString buffer) (S.Effect (getSteps ctx reader))

performRequest :: Maybe [Status] -> Request -> ClientM Response
performRequest acceptStatus req = do
  burl <- asks baseUrl
  x <- ClientM $ lift $ lift $ Codensity $ doRequest burl
  either throwError pure x
  where
    doRequest ::
      BaseUrl ->
      (Either ClientError Response -> DOM b) ->
      DOM b
    doRequest burl k = do
      resp <- fetch burl req
      headers <- parseHeaders resp

      status <- fromIntegral <$> Response.getStatus resp
      statusText <- BS.pack <$> Response.getStatusText resp
      let ok = case acceptStatus of
            Nothing -> status >= 200 && status < 300
            (Just accepted) -> status `elem` (statusCode <$> accepted)
      unless ok . fail $ show status <> ": " <> BS.unpack statusText

      reader <- createReader resp
      result <- liftIO $ runExceptT $ runSourceT reader
      case result of
        Left err -> fail err
        Right val ->
          k $
            Right
              Response
                { responseStatusCode = mkStatus status statusText,
                  responseBody = L.fromStrict (mconcat val),
                  responseHeaders = Seq.fromList headers,
                  responseHttpVersion = http11
                }

performWithStreamingRequest :: forall a. Request -> (StreamingResponse -> IO a) -> ClientM a
performWithStreamingRequest req k = do
  burl <- asks baseUrl
  ClientM $ lift $ lift $ Codensity $ doRequest burl
  where
    doRequest :: BaseUrl -> (a -> DOM b) -> DOM b
    doRequest burl k1 = do
      resp <- fetch burl req
      headers <- parseHeaders resp
      status <- fromIntegral <$> Response.getStatus resp
      statusText <- BS.pack <$> Response.getStatusText resp
      reader <- createReader resp
      x <-
        liftIO $
          k
            Response
              { responseStatusCode = mkStatus status statusText,
                responseBody = reader,
                responseHeaders = Seq.fromList headers,
                responseHttpVersion = http11
              }
      k1 x
