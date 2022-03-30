{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- Echo server program
module TrafficTest3 where

-- import Control.Concurrent (forkFinally, forkIO, threadDelay)
-- import qualified Control.Exception as E
-- import Control.Monad (unless, forever, void)
-- import qualified Data.ByteString as S
-- import Network.Socket
-- import Network.Socket.ByteString (recv, sendAll, send, sendMany, sendMsg, recvMsg)
-- import qualified Data.ByteString.Char8 as C
-- import Data.ByteString.Lazy as LB
-- import Data.Binary
-- import Prelude as P
-- import qualified Data.ByteString as BS
-- import GHC.Generics

-- bufferSize :: Int
-- bufferSize = 4096

-- start :: IO ()
-- start = do

--     forkIO  server
--     threadDelay 1000000
--     client


-- server :: IO ()
-- server = runTCPServer Nothing "3000" talk
--   where
--     talk :: (Socket, SockAddr) -> IO ()
--     talk (s, peer) = do
--         msg <- recv s bufferSize
--         unless (S.null msg) $ do
--           let bsList = BS.split 100 $ toStrict $ encode $ M $ P.replicate 100 0
--           sendMsg s peer bsList [] MSG_OOB 
--           talk (s, peer) 

-- -- sendAll :: Socket -> ByteString	-> IO ()
-- -- sendMsgSource#

-- -- sendMsg :: Socket -> SockAddr -> [ByteString] -> [Cmsg] -> MsgFlag -> IO Int

-- -- from the "network-run" package.
-- runTCPServer :: Maybe HostName -> ServiceName -> ((Socket, SockAddr) -> IO a) -> IO a
-- runTCPServer mhost port server = withSocketsDo $ do
--     addr <- resolve
--     E.bracket (open addr) close loop
--   where
--     resolve = do
--         let hints = defaultHints {
--                 addrFlags = [AI_PASSIVE]
--               , addrSocketType = Stream
--               }
--         P.head <$> getAddrInfo (Just hints) mhost (Just port)
--     open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
--         setSocketOption sock ReuseAddr 1
--         withFdSocket sock setCloseOnExecIfNeeded
--         bind sock $ addrAddress addr
--         listen sock 1
--         return sock
--     loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
--         $ \(conn, _peer) -> void $
--             -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
--             -- but 'E.bracketOnError' above will be necessary if some
--             -- non-atomic setups (e.g. spawning a subprocess to handle
--             -- @conn@) before proper cleanup of @conn@ is your case
--             forkFinally (server (conn, _peer)) (const $ gracefulClose conn 5000)


-- client :: IO ()
-- client = runTCPClient "127.0.0.1" "3000" $ \s -> do
--     -- sendAll s $ toStrict $ encode $ P.replicate 10 0
--     -- send s $ toStrict $ encode $ M $ P.replicate 100 0
--     let b = toStrict $ encode $ P.replicate 1 (0::Int)
--     sendAll s b
--     -- msg <- recv s bufferSize
--     msg <- recvMsg s 16777216 Int MsgFlag s bufferSize
--     P.putStr "Received: "
--     print (decode $ fromStrict msg :: Test)
--     C.putStrLn msg


-- -- from the "network-run" package.
-- runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
-- runTCPClient host port client = withSocketsDo $ do
--     addr <- resolve
--     E.bracket (open addr) close client
--   where
--     resolve = do
--         let hints = defaultHints { addrSocketType = Stream }
--         P.head <$> getAddrInfo (Just hints) (Just host) (Just port)
--     open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
--         connect sock $ addrAddress addr
--         return sock

-- data Test = A | B | C | M [Int]
--     deriving stock Generic
--     deriving anyclass Binary
--     deriving Show