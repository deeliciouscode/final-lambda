
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use bimap" #-}
module GraphicClient where

import LibMessage
import LibServer
--import Server_Minimal
{-
Generelles ToDo:
- introduce umgestallten
- health update message redisgnen
- wie mit clients umgehen die sich noch nicht "introduced haben"
-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.IO.Interact (Event (EventKey), Key (Char, MouseButton), KeyState (Down, Up), Modifiers (Modifiers, shift, ctrl, alt), MouseButton (LeftButton))

import Control.Concurrent (Chan, newChan, forkIO, writeChan, readChan)
import Control.Monad (void, forever, unless, (>=>))

import Network.Socket.ByteString (sendAll, recv)
import Control.Concurrent.STM (tryReadTChan, newTChan, newTChanIO, TChan, writeTChan, atomically, readTChan)
import Data.Maybe (isNothing, fromJust)
import Data.Set (Set, empty, insert, delete, elems)
import Data.Set as S
import Data.Vector.Storable as VS (Vector, empty, (!), imap, fromList, Storable, singleton, toList, take, slice, concat, length)
import Data.Map as M
import Graphics.Image.Interface (fromIx, toIx)

import Foreign.Storable.Tuple
import GHC.Float (int2Float, float2Int)

import Codec.Serialise
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy as Lazy
import Codec.Serialise.Encoding (encodeListLen)

import Data.List.Split

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
    currentMap :: Maybe (VS.Vector Int),
    currentMapList :: Maybe [[Int]],
    gridOffset :: (Float,Float)
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
            (5620,-6708)
            (M.fromList [("player", playerTile), ("op", other_Player_Tile)])
            (-1)
            (Nothing)
            Nothing
            (-160,-194)

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
            window = InWindow "WindowName" (1000,1000) (10,10) -- name size position
            fps = 60
            color = makeColor 1 1 1 1 -- Red Green Blue Alpha


render :: GameState -> IO Picture
render gs = do





    pic <- loadBMP "images/rechteck_gruen.bmp"

    playerPic <- loadBMP "images/rechteck_rot.bmp"

    m_b_l <- loadBMP "images/menu_bottom_left.bmp"
    m_b_r <- loadBMP "images/menu_bottom_right.bmp"

    m_t_l <- loadBMP "images/menu_top_left.bmp"
    m_t_r <- loadBMP "images/menu_top_right.bmp"

    m_m_t <- loadBMP "images/menu_mid_top.bmp"
    m_m_b <- loadBMP "images/menu_mid_bottom.bmp"

    --print $ currentMap gs
    if isNothing $ currentMapList gs
        then return Blank
        else do

            let tileSize = 32
                xPos = int2Float (floor (fst (position gs)/tileSize))
                yPos = int2Float (floor (snd (position gs)/tileSize))

                windowSize = 1000 :: Float
                windowOffset = int2Float $ floor ((windowSize/2) / tileSize)

                --xView = fst $ gridPosition gs
                --yView = snd $ gridPosition gs
                --inXView xx = xView <= xx && xx <= xView + int2Float (floor (1000/32))
                --inYView yy = yView >= yy && yy >= yView - int2Float (floor (1000/32))
                inXView xx = xPos - windowOffset <= xx && xx <= xPos + windowOffset
                inYView yy = yPos + windowOffset >= yy && yy >= yPos - windowOffset
                _ix x = x * 32 <= fst (position gs) + 500 && x * 32 >= fst (position gs) - 500
                _iy y = y * 32 <= snd (position gs) + 500 && y * 32 >= snd (position gs) - 500
                map = (\(Just a) -> a) $ currentMapList gs
                {-
                map = [
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [1,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
                    ,[1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [1,0]
                    , [0]
                    , [0]
                    ]
                --}
                column :: [Int] -> Float -> Float -> [Picture]
                column [] _ _ = []
                column (x:xs) xx yy
                    | _ix xx && _iy yy = translate (xx*tileSize) (yy*tileSize) (if x == 0 then pic else Blank) : column xs (xx + 1) yy
                    | otherwise = column xs (xx + 1) yy

                row [] _ = []
                row (x:xs) y = column x 0 y : row xs (y-1)

                c = pictures $ Prelude.concat $ row (map) 0
            --print $ Prelude.length map



                text t = pictures [
                                                    m_b_l,
                    translate 0 (tileSize)          m_t_l,
                    translate tileSize 0            m_m_b,
                    translate tileSize (tileSize)   m_m_t,
                    translate (2*tileSize) 0            m_m_b,
                    translate (2*tileSize) (tileSize)   m_m_t,
                    translate (3*tileSize) 0            m_m_b,
                    translate (3*tileSize) (tileSize)   m_m_t,
                    translate (4*tileSize) 0            m_m_b,
                    translate (4*tileSize) (tileSize)   m_m_t,
                    translate (5*tileSize) 0            m_m_b,
                    translate (5*tileSize) (tileSize)   m_m_t,
                    translate (6*tileSize) 0            m_b_r,
                    translate (6*tileSize) (tileSize)   m_t_r,
                    scale 0.1 0.1 $ Text  t
                    ]

             

                --player = translate (-500+16) (500-16) $ uncurry translate (position gs) playerPic

                --transX = if (xView +1 )*tileSize > xPos then int2Float (1) * int2Float (mod (float2Int xPos) (float2Int tileSize)) else 0
                --tX = fst (position gs) - 500 + int2Float (mod (float2Int (fst (position gs) - 500)) 32)
                -- 500 - 
                tX = fst $ gridOffset gs
                tY = snd $ gridOffset gs
            return $ pictures  [
                --translate (-500+tileSize/2 - xView*tileSize) (500-tileSize/2 - yView*tileSize) c ,
                --translate ( int2Float $ mod (float2Int $ fst (position gs)) 32) 0 $ translate (-500) 0 c,
                translate (-500 + 16 + (32 * tX) ) (500 -16 - tY*32) c,
                translate (-400) 0 $ text $ (show $ position gs) ++ "" ++ (show $ gridOffset gs) ,
                translate 0 0  playerPic
                ]
            --return c



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
handleInput ev@(EventKey (Char 'w') Down Modifiers {shift = _shift, ctrl = _ctrl, alt = _alt} (xC, yC)) gameState= do
    let x = fst $ position gameState
        y = snd $ position gameState
        gX = fst $ gridOffset gameState
        gY = snd $ gridOffset gameState


    return $ gameState {position = (x,y+32),gridOffset = (gX,gY+1)}
handleInput ev@(EventKey (Char 's') Down Modifiers {shift = _shift, ctrl = _ctrl, alt = _alt} (xC, yC)) gameState= do
    let x = fst $ position gameState
        y = snd $ position gameState
        gX = fst $ gridOffset gameState
        gY = snd $ gridOffset gameState
    return $ gameState {position = (x,y-32), gridOffset = (gX,gY-1)}
handleInput ev@(EventKey (Char 'a') Down Modifiers {shift = _shift, ctrl = _ctrl, alt = _alt} (xC, yC)) gameState= do
    let x = fst $ position gameState
        y = snd $ position gameState

        gX = fst $ gridOffset gameState
        gY = snd $ gridOffset gameState
       
    
    return $ gameState {position = (x-32,y), gridOffset = (gX+1,gY)}
handleInput ev@(EventKey (Char 'd') Down Modifiers {shift = _shift, ctrl = _ctrl, alt = _alt} (xC, yC)) gameState= do
    let x = fst $ position gameState
        y = snd $ position gameState

        gX = fst $ gridOffset gameState
        gY = snd $ gridOffset gameState
    return $ gameState {position = (x+32,y), gridOffset = (gX-1,gY)}
handleInput ev@(EventKey (Char c) Down Modifiers {shift = _shift, ctrl = _ctrl, alt = _alt} (xC, yC)) gameState= do
    --print ev
    return gameState -- $ gameState {list_key = S.insert c $ list_key gameState}
handleInput (EventKey (Char c) Up Modifiers {shift = _shift, ctrl = _ctrl, alt = _alt} (xC, yC)) gameState= do
    --print gameState -- $ position gameState
    return gameState-- $ gameState {list_key = S.delete c $ list_key gameState}
handleInput (EventKey (MouseButton LeftButton) Up _ (x,y)) gameState = do

    --let a = 1 + floor ((y - 500)/32)
    print  (floor $ (x + 500)/32, abs $ 1 + floor ((y - 500)/32))

    return gameState

handleInput e gameState = do
    --print e
    return gameState

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

                                    let a = chunksOf 1000 $ VS.toList vector

                                    --return gs {currentMap = Just vector}
                                    return gs {currentMapList = Just a}

                                Message [Source Server] (WrapList l) -> do
                                    --print $ "list" ++ show l
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


