{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Client.JSaddleSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
  ( newEmptyMVar,
    putMVar,
    takeMVar,
  )
import Control.Exception
  ( handle,
    throwIO,
  )
import Control.Monad (forM_)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as B
import Data.Proxy (Proxy (..))
import Data.String (IsString)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (diffUTCTime)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHCJS.DOM.Types (DOM)
import qualified Language.Javascript.JSaddle.Run as Run
import qualified Language.Javascript.JSaddle.WebSockets as WS
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp (testWithApplication)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors
  ( simpleCors,
  )
import Network.WebSockets
  ( defaultConnectionOptions,
  )
import qualified Network.WebSockets as WS
import Servant.API
  ( JSON,
    NewlineFraming,
    OctetStream,
    Post,
    ReqBody,
    SourceIO,
    StreamPost,
    type (:>),
  )
import qualified Servant.Client.JSaddle as Client
  ( BaseUrl
      ( BaseUrl,
        baseUrlHost,
        baseUrlPath,
        baseUrlPort,
        baseUrlScheme
      ),
    ClientError,
    ClientM,
    Scheme (Http),
    client,
    mkClientEnv,
    withClientM,
  )
import Servant.Server
  ( Application,
    Handler,
    HasServer (ServerT),
    serve,
  )
import Servant.Types.SourceT (SourceT (SourceT), StepT (..), unSourceT)
import qualified Servant.Types.SourceT as S
import qualified System.Process as P
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

type TestNonStreamingApi =
  ReqBody '[OctetStream] B.ByteString
    :> Post '[JSON] TestResponse

testNonStreamingApi :: Proxy TestNonStreamingApi
testNonStreamingApi = Proxy

data TestResponse = TestResponse {byteList :: [Word8]}
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

type TestStreamingApi =
  ReqBody '[OctetStream] B.ByteString
    :> StreamPost NewlineFraming JSON (SourceIO Word8)

testStreamingApi :: Proxy TestStreamingApi
testStreamingApi = Proxy

spec :: Spec
spec = do
  describe "Servant.Client.JSaddle" $ do
    it "Send and receive a mix of valid utf-8 and non-utf8 bytes" $
      let server x = pure . TestResponse . B.unpack $ x
          bytes = [0x01, 0xff, 0x02, 0xfe, 0x03, 0xfd, 0x00, 0x64, 0xc3, 0xbb, 0x68, 0xc3]
          client = Client.client testNonStreamingApi (B.pack bytes)
          evaluateResponse response = pure $ response `shouldBe` Right (TestResponse bytes)
       in runTest testNonStreamingApi server client evaluateResponse

    it "Send and receive a mix of valid utf-8 and non-utf8 bytes over HTTP/2 streaming" $
      let delay :: (Integral a) => a
          delay = 500 * 1000
          testStreamingServer bs = pure $ SourceT ($ stepsFrom (B.unpack bs))
            where
              stepsFrom :: [Word8] -> StepT IO Word8
              stepsFrom [] = Stop
              stepsFrom (x : xs) =
                Yield x $
                  Effect (threadDelay delay >> pure (stepsFrom xs))

          bytes = [0x01, 0xff, 0x02, 0xfe]
          client = Client.client testStreamingApi (B.pack bytes)

          evaluateResponse (Left err) = pure $ expectationFailure (show err)
          evaluateResponse (Right src) = liftIO $ do
            result <- unSourceT src (readData [] [])
            pure $ do
              case result of
                Left err -> expectationFailure err
                Right (responses, timestamps) -> do
                  -- check response contents
                  responses `shouldBe` bytes

                  -- check response timestamps
                  -- each chunk should arrive with a delay ~ `delay`
                  forM_ (zip timestamps (tail timestamps)) $ \(t1, t2) -> do
                    let dt = diffUTCTime t2 t1
                    (dt - fromInteger delay / 1000000) `shouldSatisfy` (< 0.1)
            where
              readData ::
                [Word8] ->
                [UTCTime] ->
                StepT IO Word8 ->
                IO (Either String ([Word8], [UTCTime]))
              readData _ _ (S.Error err) = pure $ Left err
              readData responses timestamps S.Stop = pure $ Right (reverse responses, reverse timestamps)
              readData responses timestamps (S.Skip s) = readData responses timestamps s
              readData responses timestamps (S.Effect eff) = eff >>= readData responses timestamps
              readData responses timestamps (S.Yield resp s) = do
                time <- getCurrentTime
                readData (resp : responses) (time : timestamps) s
       in runTest testStreamingApi testStreamingServer client evaluateResponse

runTest ::
  HasServer api '[] =>
  Proxy api ->
  ServerT api Handler ->
  Client.ClientM a1 ->
  (Either Client.ClientError a1 -> DOM (IO ())) ->
  IO ()
runTest api server client evaluateResponse = do
  done <- newEmptyMVar
  result <- newEmptyMVar

  -- How this work:
  --
  -- 1. we start server warp, which serves simple API
  -- 2. we start client warp, which serves jsaddle running the 'action'
  -- 3. we run google-chrome-stable to open jsaddle page to run the test

  let action :: Int -> DOM ()
      action serverPort = do
        result' <- Client.withClientM client clientEnv evaluateResponse
        liftIO $ putMVar result result'
        liftIO $ putMVar done ()
        where
          clientEnv =
            Client.mkClientEnv
              Client.BaseUrl
                { Client.baseUrlScheme = Client.Http,
                  Client.baseUrlHost = "localhost",
                  Client.baseUrlPort = fromIntegral serverPort,
                  Client.baseUrlPath = "/"
                }

  let addCors :: Wai.Middleware
      addCors app request respond =
        if Wai.requestMethod request == "OPTIONS"
          then respond $ Wai.responseLBS Http.status200 corsHeaders ""
          else addHeaders corsHeaders app request respond
        where
          corsHeaders :: (IsString s1, IsString s2) => [(s1, s2)]
          corsHeaders =
            [ ("Access-Control-Allow-Origin", "*"),
              ("Access-Control-Allow-Methods", "POST"),
              ("Access-Control-Allow-Headers", "content-type")
            ]

      logRequest :: Wai.Middleware
      logRequest app request respond = do
        putStrLn "Request"
        print request
        app request $ \response -> do
          putStrLn "Response Headers"
          mapM_ print (Wai.responseHeaders response)
          respond response

      serverApp :: IO Application
      serverApp = pure $ logRequest $ addCors $ serve api server

  let handler :: WS.ConnectionException -> IO ()
      handler WS.ConnectionClosed = return ()
      handler e = throwIO e

  handle handler $
    Warp.testWithApplication serverApp $ \serverPort -> do
      let clientApp :: IO Application
          clientApp =
            WS.jsaddleOr
              defaultConnectionOptions
              (action serverPort >> Run.syncPoint)
              WS.jsaddleApp

      Warp.testWithApplication (simpleCors <$> clientApp) $ \clientPort -> do
        putStrLn $ "server http://localhost:" ++ show serverPort
        putStrLn $ "client http://localhost:" ++ show clientPort
        putStrLn $ "google-chrome-stable --headless --disable-gpu --screenshot http://localhost:" ++ show clientPort

        -- Run headless chrome
        -- https://docs.travis-ci.com/user/gui-and-headless-browsers/#using-the-chrome-addon-in-the-headless-mode
        -- https://developers.google.com/web/updates/2017/04/headless-chrome
        hdl <-
          P.spawnProcess
            "google-chrome-stable"
            [ "--headless",
              "--disable-gpu",
              "--remote-debugging-port=9222",
              "http://localhost:" ++ show clientPort
            ]

        takeMVar done >> P.terminateProcess hdl

  result' <- takeMVar result
  result'
