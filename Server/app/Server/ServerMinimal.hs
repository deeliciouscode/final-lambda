{-# LANGUAGE DerivingStrategies #-}
module Server.ServerMinimal where

import Server.LibMessage
import Server.LibServer

import System.Time

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
import Control.Concurrent (
    forkFinally,
    newEmptyMVar,
    MVar,
    readMVar,
    swapMVar,
    newChan,
    Chan,
    writeChan,
    readChan,
    forkIO, modifyMVar_, newMVar, takeMVar, putMVar, threadDelay)
import Network.Socket.ByteString (sendAll, recv)
import Data.ByteString.Char8 (unpack, pack, ByteString)
import qualified Data.ByteString  as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Control.Monad (unless, forever, void, when)
import GHC.Generics (Generic)
import Data.Binary
import Dungeons.API

import Data.Map as M
import Data.Vector.Storable as VS (empty, Vector, fromList, toList)
--import Data.Vector.Storable as VS
----------------------------------------------------------
-- "Frei interpretiert" nach: 
-- https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html 
----------------------------------------------------------


----------------------------------------------------------
-- ToDo:
-- 
-- Bei Client Disconect: Client aus connectionList (IO (MVar [Socket])) austragen -> eventuell zukünftig als dict umsetzen?
--
--
----------------------------------------------------------
-- main :: IO ()
-- main = server

bufferSize :: Int
bufferSize = 10000000

server :: IO b
server   = do
    print "server is running..."
    --connectionList <- newEmptyMVar :: IO (MVar [Socket]) 
    connectionList <- newMVar [] :: IO (MVar [Socket])
    messageQueue <- newChan :: IO (Chan Message)

    playerList <- newMVar M.empty  :: IO (MVar  (Map Int PlayerInfo)) -- sollte verm. in zukunft mit connectionList zusammengelegt werden
        
    ----------------------------------------------------------
    -- sendet nextMessage aus Channel an alle Clients
    ----------------------------------------------------------
    forkIO $ forever $ do

        Message l p <- readChan messageQueue
        map_playerList <- readMVar  playerList
        list_connection <- readMVar connectionList
        --print p
        case l of
            [ConnectionWrapper conn] -> do
                
                
                sendAll conn $ toByteString $ Message [Source Server] p
                --threadDelay 1
                --sendAll conn $ toByteString $ Message [Source Server] (MapVector $ VS.fromList $ replicate (1000*1000) 0)
                
                

            [Source (Client id), Target (Map i)] -> do
                let player = case M.lookup id map_playerList of
                                Just pI -> pI
                                _ -> PI (-1) (-1,-1) (0,0) (0,0) (-1)
                    newPlayerList = M.insert id (player {pI_mapID = i}) map_playerList
                swapMVar playerList newPlayerList
                readMVar connectionList >>= mapM_ (`sendAll` toByteString  (Message [Source Server] (PlayerInformation newPlayerList)))




            [Source (Client id)] -> do
                let player = case M.lookup id map_playerList of
                                Just pI -> pI
                                _ -> PI (-1) (-1,-1) (0,0) (0,0) (-1)

                case p of
                    PositionUpdate (x,y)    -> do
                        
                        
                        
                        void $ swapMVar playerList $ M.insert id (player {pI_position = (x,y)}) map_playerList
                    _                       -> putMVar playerList map_playerList


                readMVar connectionList >>= mapM_ (`sendAll` toByteString  (Message l p))

            [Source Server] -> do
                readMVar connectionList >>= mapM_ (`sendAll` toByteString  (Message l p))

            notImplementedMessage -> do
                --putMVar playerList map_playerList
                print $ "Message: " ++ show notImplementedMessage ++  "not yet supportet"


    ----------------------------------------------------------
    -- sendet input auf ServerKonsole an alle clients
    ----------------------------------------------------------
    forkIO $ forever $ do
        input <- getLine

        if input == "spawn map" 
            then do
                print "OK spawning map"
                (dim, dungeon, meta) <- getDungeon 420
                writeChan messageQueue $ Message [Source Server] $ MapVector dungeon


            else print input 
        
        --writeChan messageQueue $ Message [Source Server] $ M input
    -- send 
    -- forkIO $ forever $ do
    --     threadDelay $ round (1000000 / 60)
    --     playerList <- readMVar playerList 
    --     message <- getMsg
        
    --     readMVar connectionList >>= mapM_ (`sendAll` toByteString  (Message [Source Server] (MapBotPosition M.empty))) -- bot positions
    --     readMVar connectionList >>= mapM_ (`sendAll` toByteString  (Message [Source Server] (MapBotPosition M.empty))) -- bot actions
        
    --     print "test"

    {-
    forkIO $ forever $ do
        threadDelay $ round (1000000 / 60)
        (dims, vector, meta) <- getDungeon 420
        -- print dims
        return ()
    -}
   

    ----------------------------------------------------------
    -- startet Server
    ----------------------------------------------------------
    runTCPServer Nothing "3000" connectionList messageQueue




