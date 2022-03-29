{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Server_Test where
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent (forkFinally, forkIO)
import Control.Monad (unless, forever, void, forM_, when)
import qualified Data.ByteString as S


import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Socket.ByteString.Lazy  as Lazy
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (fromStrict, ByteString, pack)
import Data.Binary (Word8, encode, Binary, decode)
import GHC.Generics (Generic)

import Data.Word (Word16)
import Control.Exception (assert)
import Debug.Trace
import Text.Printf
import           "monad-loops" Control.Monad.Loops  (unfoldrM)
import Data.Int
start = do

    forkIO server
    client
newtype MsgLen = MsgLen Word16
lenLen = 2 :: Int64
msgLenEOT = MsgLen 0
decodeXmitLen :: ByteString -> Int64
decodeXmitLen bs =
      assert (BS.length bs == lenLen) $
          let w = decode bs :: Word16
          in fromIntegral w
encodeXmitLen :: MsgLen -> ByteString
encodeXmitLen (MsgLen w) = encode w

sendEOT :: Socket -> IO ()
sendEOT skt = do
        Lazy.send skt $ encodeXmitLen msgLenEOT
        return ()

maxlen = 0x0FFFF :: Int64

validateMsgLen :: Int64 -> Maybe MsgLen
validateMsgLen len = if len <= maxlen && len >= 0
                        then Just $ MsgLen $ fromIntegral len
                        else Nothing

sendAsBS :: Binary a => Socket -> a -> IO ()
sendAsBS skt myData =
    case validateMsgLen len of
      Nothing -> traceIO $ -- error as trace msg
                    printf "sendAsBS: msg length %d out of range" len

      Just msgLen -> do
        Lazy.send skt $ encodeXmitLen msgLen
        bytes <- Lazy.send skt bsMsg
        when (bytes /= len) $ traceIO "sendAsBS: bytes sent disagreement"
        -- return ()
  where
    bsMsg = encode myData
    len = BS.length bsMsg

isLenEOT :: Int64 -> Bool
isLenEOT = (== 0)

type UnfoldableM m b a = Monad m => b -> m (Maybe (a, b))
receiveMay :: Binary a => Socket -> UnfoldableM IO b a
receiveMay skt st = do
        len <- decodeXmitLen <$> Lazy.recv skt lenLen
        if isLenEOT len
           then return Nothing
           else do
             bs <- Lazy.recv skt len
             return $ Just (decode bs, st)

server :: IO ()
server = runTCPServer Nothing "3000" 

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName ->   IO a
runTCPServer mhost port  = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> do
            res <-unfoldrM (receiveMay conn) 0 :: IO String --IO [XmitType]
            forM_ res print
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            --forkFinally (server conn) (const $ gracefulClose conn 5000)


data Foo = F | A | M String | L [Int]
    deriving stock Generic
    deriving anyclass Binary
    deriving Show

type XmitType = Int32
client ::  IO ()
client  = runTCPClient "127.0.0.1" "3000" $ \s -> do
    i <- getLine
    let list = replicate 1000000 0
        testList = [1..4] :: [XmitType]
    --Lazy.sendAll s $ encode $ L list
    --forM_ testList $ \n -> sendAsBS s n
    sendAsBS s "$ L [1..10]"
    sendEOT s
    client


-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock