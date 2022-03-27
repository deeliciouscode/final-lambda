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
      Payload(SetID) )
import qualified Data.ByteString  as BS
import Control.Monad ( forever, unless )
import Network.Socket.ByteString ( recv )

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

