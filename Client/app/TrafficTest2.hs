{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module TrafficTest2 where




import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S



import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
--import Network.Socket.ByteString.Lazy as Lazy
import GHC.Generics
import Data.Binary
import Data.ByteString.Lazy as LB
import GHC.Int (Int64)

start :: IO ()
start = do

    forkIO  server
    line <- getLine
    print line
    client

buffer :: Int
buffer = 100000000

server :: IO ()
server = runTCPServer Nothing "3000" talk
  where
    talk s = do
        let a = fromIntegral
        msg <- recv s  buffer
        print (decode $ fromStrict msg :: Test)
        talk s
        {-
        unless (LB.null msg) $ do
            print "foo"
            let m :: Test
                m = decode msg
            print m
            talk s
            -}

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        Prelude.head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock  buffer
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 10000)



client :: IO ()
client = runTCPClient "127.0.0.1" "3000" $ \s -> do loop s
    where
        loop s= do
            sendAll s $ toStrict$ encode $ M $ Prelude.replicate 10 0
            threadDelay 1000000
            sendAll s $ toStrict$ encode B
            threadDelay 1000000
            sendAll s $ toStrict$ encode  A
            threadDelay 1000000
            sendAll s $ toStrict $ encode $ M $ Prelude.replicate (1000*1000) 3
            threadDelay 1000000
            --Lazy.sendAll s $ encode $ M $ Prelude.replicate 10 1
            --Lazy.sendAll s $ encode $ M $ Prelude.replicate 10 2
            --Lazy.sendAll s $ encode $ M $ Prelude.replicate 10 2
            i <- getLine 
            loop s
    

-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        Prelude.head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

data Test = A | B | C | M [Int]
    deriving stock Generic
    deriving anyclass Binary
    deriving Show