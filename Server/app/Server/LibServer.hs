module Server.LibServer where

import Network.Socket
    ( setCloseOnExecIfNeeded,
      defaultHints,
      getAddrInfo,
      openSocket,
      withSocketsDo,
      setSocketOption,
      bind,
      listen,
      close,
      withFdSocket,
      AddrInfo(
          addrFlags,
          addrSocketType,
          addrAddress),
      AddrInfoFlag(AI_PASSIVE),
      HostName,
      ServiceName,
      SocketOption(ReuseAddr),
      SocketType(Stream),
      accept,
      gracefulClose, Socket, connect )
import Control.Exception (bracket, bracketOnError)
import Control.Concurrent
    ( forkFinally, writeChan, swapMVar, readMVar, Chan, MVar )
import Server.LibMessage
    ( fromByteString,
      Destionation(ConnectionWrapper),
      Message(..),
      Payload(SetID), SerialiseTest (..) )
import qualified Data.ByteString  as BS
import Control.Monad ( forever, unless )
import Network.Socket.ByteString ( recv , sendAll)

import Server.LibMessage(toByteString, Payload (..))

import Codec.Serialise
import Data.ByteString.Lazy (toStrict)

runTCPServer :: Maybe HostName -> ServiceName -> MVar [Socket] -> Chan Message -> IO a2
runTCPServer host port mV messageQueue = withSocketsDo $ do
    addr <- resolve
    bracket (open addr) close  (`loop` 1)
    where
        resolve :: IO AddrInfo
        resolve = do
            let hints = defaultHints {
                    addrFlags = [AI_PASSIVE]
                ,   addrSocketType = Stream
                }
            head <$> getAddrInfo (Just hints) host (Just port)
        -- bracketOnError (aquire resources) error (work with resources)
        open addr = bracketOnError (openSocket addr) close $ \sock -> do
            setSocketOption sock ReuseAddr 1
            withFdSocket sock setCloseOnExecIfNeeded
            bind sock $ addrAddress addr
            listen sock 1024
            return sock

        loop sock id =  do
                        bracketOnError (accept sock) (close . fst) $ \(conn, _peer) -> do
                            
                            connections <- readMVar mV
                            swapMVar mV $ connections ++ [conn]

                            --sendAll conn $ toByteString $ Message [Source Server] $ SetID id
                            -- work in progress: 
                            --sendAll conn $ toByteString $ Message [] (WrapList $ replicate 200 0)
                            let l :: [Int]
                                l = [   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,60,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,80,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,120,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,140,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,160,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,180,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,200,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,220,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,240,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,260,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,280,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,300]
                                a = serialise l
                                t = toStrict $ serialise $ L $ replicate (1000*1000) 0

                            --sendAll conn $ serialise (L [])

                            
                            sendAll conn $ toStrict $ serialise $ L $ replicate 1000000 0
                            sendAll conn $ toStrict $ serialise $ L $l
                            --sendAll conn $ toStrict $ serialise $ L l
                            writeChan messageQueue $ Message [ConnectionWrapper conn] $ SetID id
                            forkFinally (forever $ do
                                msg <- recv conn 1024
                                unless (BS.null msg) $ writeChan messageQueue $ fromByteString msg
                                --unless (BS.null msg) $ print $ fromByteString msg
                                ) 
                                (do
                                    ------------------------------------------
                                    -- Clear unused connection from connectionList
                                    -- ToDo: funktioniert nicht u.o. wies soll (=gedacht ist...)
                                    ------------------------------------------
                                    let search [] = []
                                        search (c:xs)
                                            | c == conn = search xs
                                            | otherwise = c : search xs

                                    --const $ modifyMVar_ mV (return . search)
                                    const $ gracefulClose conn 5000
                                )

                        loop sock (id + 1)




type Action a = Socket -> IO a
runTCPClient :: HostName -> ServiceName -> Action a -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    bracket (open addr) close client
  where
    resolve :: IO AddrInfo 
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        Prelude.head <$> getAddrInfo (Just hints) (Just host) (Just port)
    
    open :: AddrInfo -> IO Socket
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

