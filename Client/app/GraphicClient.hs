{-
Generelles ToDo:
- introduce umgestallten
- health update message redisgnen
- wie mit clients umgehen die sich noch nicht "introduced haben"
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use bimap" #-}
module GraphicClient where

import LibMessage
import LibServer
--import Server_Minimal


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.IO.Interact (Event (EventKey), Key (Char), KeyState (Down, Up), Modifiers (Modifiers, shift, ctrl, alt))

import Control.Concurrent (Chan, newChan, forkIO, writeChan, readChan)
import Control.Monad (void, forever, unless, (>=>))

import Network.Socket.ByteString (sendAll, recv)
import Control.Concurrent.STM (tryReadTChan, newTChan, newTChanIO, TChan, writeTChan, atomically, readTChan)
import Data.Maybe (isNothing)
import Data.Set (Set, empty, insert, delete, elems)
import Data.Set as S
import Data.Vector.Storable as VS (Vector, empty, (!), imap, fromList, Storable, singleton, toList)
import Data.Map as M
import Graphics.Image.Interface (fromIx, toIx)

import Foreign.Storable.Tuple
import GHC.Float (int2Float)

import Codec.Serialise
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy as Lazy
import Codec.Serialise.Encoding (encodeListLen)

data GameState = GS {
    serverMessages :: TChan Message,
    clientMessages :: TChan Message,
    clientID :: Maybe Int,
    alivePingTime :: Float,
    list_OtherPlayer :: Map Int PlayerInfo,
    list_key :: Set Char,
    position :: (Float, Float),
    list_Tile :: Map String Picture,
    mapID :: Int,
    currentMap :: Maybe (VS.Vector Int)

    } 



-- main :: IO ()
-- main = do
runClient :: IO ()
runClient = do

    -- ToDo:
    -- Aufpassen: TChan eigentlich gedacht für unsafePerformIO
    -- https://stackoverflow.com/questions/10529284/is-there-ever-a-good-reason-to-use-unsafeperformio
    serverMessages <- atomically newTChan :: IO (TChan Message)
    clientMessages <- atomically newTChan :: IO (TChan Message)

    playerTile <- loadBMP "images/rechteck_gruen.bmp"
    other_Player_Tile <- loadBMP  "images/rechteck_rot.bmp"

    let initialState = GS
            serverMessages
            clientMessages
            Nothing
            0
            (M.empty :: Map Int PlayerInfo)
            (S.empty :: Set Char)
            (0,0)
            (M.fromList [("player", playerTile), ("op", other_Player_Tile)])
            (-1)
            (Nothing)

    forkIO $ runTCPClient "127.0.0.1" "3000" $ \socket -> do
        void $ forkIO $ forever $ do
            msg <- recv socket bufferSize
            let d_msg = fromByteString  msg
            --print d_msg
            atomically $ writeTChan serverMessages d_msg
            

        void $ forever $ do
            msg <- atomically $ readTChan clientMessages
            sendAll socket $ toByteString msg


    playIO
       window
       color
       fps
       initialState --initialState
       render
       handleInput
       onUpdate


        where
            window = InWindow "WindowName" (300,300) (10,10) -- name size position
            fps = 60
            color = makeColor 1 1 1 1 -- Red Green Blue Alpha


render :: GameState -> IO Picture
render gs = do
    

    

        
    pic <- loadBMP "images/rechteck_gruen.bmp"
    --print $ currentMap gs
    {-
    let     
            rowLength = 20
            getTile tileIndex = pic -- dummy
            tileList =  Prelude.map 
                            --(\((x,y), tileIndex) -> translate (8 * int2Float x) (8 * int2Float y) $ getTile tileIndex) $ 
                            (\((x,y), tileIndex) -> translate 0 0  pic) $
                        VS.toList $ 
                            VS.imap 
                                (\index value -> (toIx rowLength index ,value)) $ 
                            currentMap gs
            picture = pictures tileList

            debugText = show (mapID gs) ++ show (position gs)
            foo = show $ M.toList $ list_OtherPlayer gs
    print $length $ VS.toList $ currentMap gs
    -- return $ translate (-100) 0 $ scale 0.1 0.1 $ Text foo
    -}

    --print $ isNothing $ currentMap gs
    return pic


handleInput :: Event -> GameState -> IO GameState
handleInput ev@(EventKey (Char '+') Down Modifiers {shift = _shift, ctrl = _ctrl, alt = _alt} (xC, yC)) gameState= do
    let cID = clientID gameState

    if isNothing cID 
        then return gameState
        else do
            let playerID = (\(Just id) -> id) $ clientID gameState
                player = (\(Just id) -> id) $ M.lookup playerID $ list_OtherPlayer gameState
                mapID = pI_mapID player
                --newPlayerList = M.insert playerID (player {pI_mapID = mapID + 1}) $ list_OtherPlayer gameState
            atomically $ writeTChan (clientMessages gameState) $ Message [Source $ Client playerID, Target (Map (mapID + 1))] Null

            return gameState --{list_OtherPlayer = newPlayerList}

handleInput ev@(EventKey (Char '-') Down Modifiers {shift = _shift, ctrl = _ctrl, alt = _alt} (xC, yC)) gameState= do
    let cID = clientID gameState

    if isNothing cID 
        then return gameState
        else do
            let playerID = (\(Just id) -> id) $ clientID gameState
                player = (\(Just id) -> id) $ M.lookup playerID $ list_OtherPlayer gameState
                mapID = pI_mapID player
                --newPlayerList = M.insert playerID (player {pI_mapID = mapID + 1}) $ list_OtherPlayer gameState
            atomically $ writeTChan (clientMessages gameState) $ Message [Source $ Client playerID, Target (Map (mapID - 1))] Null

            return gameState --{list_OtherPlayer = newPlayerList}

handleInput ev@(EventKey (Char c) Down Modifiers {shift = _shift, ctrl = _ctrl, alt = _alt} (xC, yC)) gameState= do
    --print ev
    return $ gameState {list_key = S.insert c $ list_key gameState}
handleInput (EventKey (Char c) Up Modifiers {shift = _shift, ctrl = _ctrl, alt = _alt} (xC, yC)) gameState= do
    return $ gameState {list_key = S.delete c $ list_key gameState}
handleInput e gameState = return gameState

onUpdate :: Float -> GameState -> IO GameState
onUpdate delta _gameState =

    let
        sendPosition :: GameState -> IO GameState
        sendPosition gs
            | isNothing $ clientID gs = return gs
            | otherwise = do
                -- print "test"
                let cID =  (\(Just i) -> i) $ clientID gs
                unless (S.null $ list_key gs) $ atomically $ writeTChan (clientMessages gs) $ Message [Source $ Client cID] $ PositionUpdate $ position gs


                return $ gs {alivePingTime = 0}


        handleActivePing :: GameState -> IO GameState
        handleActivePing gs
            | isNothing $ clientID gs = return gs
            {--
            | alivePingTime gs > 0.5 = do
                let cID =  (\(Just i) -> i) $ clientID gs
                atomically $ writeTChan (clientMessages gs) $ Message [Source $ Client cID] Alive


                return $ gs {alivePingTime = 0} --}
            | otherwise = return $ gs {alivePingTime = alivePingTime gs + delta} :: IO GameState

        handleServerMessages :: GameState -> IO GameState
        handleServerMessages gs = do
            mServerMessage <- atomically $ tryReadTChan $ serverMessages gs
            if isNothing mServerMessage
                then return gs
                else do
                    let isID (Just (Message _ (SetID _))) = True
                        isID _ = False
                    if      isNothing (clientID gs) 
                        &&  not ( isID mServerMessage) 
                        then do
                            print $ "noId" ++ show mServerMessage
                            return gs
                        else do
                            case (\(Just msg) -> msg) mServerMessage of
                                -- hier müsste verm. auch ein map handling stattfinden
                                Message [Source Server] (SetID i) -> do
                                    -- unter umständen muss respnse gestored werden wenn server antwort zu schnell kommt

                                    atomically $ writeTChan (clientMessages gs) $ Message [Source (Client i), Target (Map $ mapID gs)] Null 
                                    return $ gs {clientID = Just i} 

                                Message [Source (Client i)] (PositionUpdate (x,y)) -> do
                                    let updatedPlayer = case M.lookup i $ list_OtherPlayer gs of
                                                    Just player -> M.insert i player {pI_position = (x,y)} $ list_OtherPlayer gs
                                                    Nothing -> M.insert i (PI (-1) (-1,-1) (0,0) (0,0) (-1)) $ list_OtherPlayer gs
                              
                                    return gs {list_OtherPlayer = updatedPlayer}-- {list_OtherPlayer = M.insert i updatedPlayer $ list_OtherPlayer gs}
                                    
                                Message [Source Server] (PlayerInformation m) -> return gs {list_OtherPlayer = m}
                                
                                Message [Source Server] (MapVector vector) -> do
                                    print "mapVector inc"
                                    return gs {currentMap = Just vector}
                                
                                Message [Source Server] (WrapList l) -> do
                                    print $ "list" ++ show l
                                    return gs

                                
                                p -> do
                                    print "else"
                                    return gs



        procedureInput :: GameState -> IO GameState
        procedureInput gs = do

            let -- IO für: wenn man auch IO Aktionen aufgrund von keys in Liste ausführen möchte
                -- (soweit sie noch nicht in handleInput abgearbeitet wurden)
                work :: [Char] -> (Float,Float) -> IO (Float,Float)
                work [] p = return p
                work ('a':xs) (x,y) = work xs (x-1,y)
                work ('d':xs) (x,y) = work xs (x+1,y)
                work ('w':xs) (x,y) = work xs (x,y+1)
                work ('s':xs) (x,y) = work xs (x,y-1)

                work (_:xs) p = work xs p

            keyResult <- work (S.elems $ list_key gs) (position gs)
            return $ gs {position = keyResult}

    in  (       sendPosition
        >=>     handleActivePing
        >=>     handleServerMessages
        >=>     procedureInput
        ) _gameState


