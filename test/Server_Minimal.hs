{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Server_Minimal where

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
    forkIO, modifyMVar_, newMVar, takeMVar, putMVar)
import Network.Socket.ByteString (sendAll, recv)
import Data.ByteString.Char8 (unpack, pack, ByteString)
import qualified Data.ByteString  as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Control.Monad (unless, forever, void)
import GHC.Generics (Generic)
import Data.Binary

import Data.Map as M
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
--main :: IO ()
--main = server

data PlayerInfo = PI {
    pI_mapID :: Int, -- -1 als not set
    pI_health :: (Float, Float), -- (-1,-1) als not set
    pI_position :: (Float, Float)
}
    deriving stock Generic 
    deriving anyclass Binary
    deriving Show
    deriving Eq

server   = do
    --connectionList <- newEmptyMVar :: IO (MVar [Socket]) 
    connectionList <- newMVar [] :: IO (MVar [Socket]) 
    messageQueue <- newChan :: IO (Chan Message) 

    playerList <- newMVar M.empty  :: IO (MVar  (Map Int PlayerInfo)  ) -- sollte verm. in zukunft mit connectionList zusammengelegt werden

    ----------------------------------------------------------
    -- sendet nextMessage aus Channel an alle Clients
    ----------------------------------------------------------
    forkIO $ forever $ do

        Message l p <- readChan messageQueue
        map_playerList <- readMVar  playerList
        list_connection <- readMVar connectionList
        print p
        case l of 
            [ConnectionWrapper conn] -> sendAll conn $ toByteString $ Message [Source Server] p  

            [Source (Client id), Target (Map i)] -> do
                let player = case M.lookup id map_playerList of 
                                Just pI -> pI 
                                _ -> PI (-1) (-1,-1) (0,0)
                    newPlayerList = M.insert id (player {pI_mapID = i}) map_playerList
                swapMVar playerList newPlayerList
                readMVar connectionList >>= mapM_ (`sendAll` toByteString  (Message [Source Server] (PlayerInformation newPlayerList)))

            [Source (Client id)] -> do
                let player = case M.lookup id map_playerList of 
                                Just pI -> pI 
                                _ -> PI (-1) (-1,-1) (0,0)

                case p of 
                    PositionUpdate (x,y)    -> void $ swapMVar playerList $ M.insert id (player {pI_position = (x,y)}) map_playerList        
                    _                       -> putMVar playerList map_playerList


                readMVar connectionList >>= mapM_ (`sendAll` toByteString  (Message l p))
            
            notImplementedMessage -> do
                --putMVar playerList map_playerList
                print $ "Message: " ++ show notImplementedMessage ++  "not yet supportet"
                

       
   

    ----------------------------------------------------------
    -- sendet input auf ServerKonsole an alle clients
    ----------------------------------------------------------
    forkIO $ forever $ do
        input <- getLine 
        print input
        --writeChan messageQueue $ Message [Source Server] $ M input


    ----------------------------------------------------------
    -- startet Server
    ----------------------------------------------------------
    runTCPServer Nothing "3000" connectionList messageQueue




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





instance Binary Socket where
    put = error "wrong wrapper use" 
    get = error "wrong wrapper use" 

data Destionation = 
        Target SubDestination    
    |   Source SubDestination   
    |   ConnectionWrapper Socket   
    deriving stock Generic 
    deriving anyclass Binary
    deriving Show
    deriving Eq
data SubDestination = 
        Client Int 
    |   Server
    |   Broadcast
    |   Map Int
    deriving stock Generic 
    deriving anyclass Binary
    deriving Show
    deriving Eq

data Message =  Message [Destionation] Payload                                 
    deriving stock Generic 
    deriving anyclass Binary
    deriving Show
    deriving Eq

data Payload = 
                    SetID Int                                       -- initiale ID an Client
                |   PositionUpdate (Float, Float)
                |   Action Int                                      -- ActionID
                |   HealthChanged Float                             -- wenn positiv: dann Heal
                                                                    -- wenn negativ: damage
                |   EffectGained Int                                -- Effect ID
                |   EffectWanished Int                              -- Effect ID
                |   M String                                   -- Map ID + player update?                               -- 
                |   PlayerInformation (Map Int PlayerInfo)
                |   GetMyInfo                                       -- Ruft stored PlayerInfo aus DataBase ab
                                                                    -- ebenfalls trigger für PlayerInformation
                |   Null
    deriving stock Generic 
    deriving anyclass Binary
    deriving Show
    deriving Eq


--depricated:------------------------------------------
data PlayerRepresentation = PR {
    pRclientID :: Int,
    pRmapID :: Int,
    pRhealth :: (Float, Float),
    pRcoord :: (Float, Float)
}
    deriving stock Generic 
    deriving anyclass Binary
    deriving Show
    deriving Eq
-------------------------------------------------------


fromByteString :: ByteString -> Message
fromByteString bS = decode $ fromStrict bS

toByteString :: Message -> ByteString
toByteString msg = toStrict $ encode msg