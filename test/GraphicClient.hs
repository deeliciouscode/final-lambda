{-
Generelles ToDo:
- introduce umgestallten
- health update message redisgnen
- wie mit clients umgehen die sich noch nicht "introduced haben"
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}



import Server_Minimal


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

import Data.Map as M

data GameState = GS {
    serverMessages :: TChan Message,
    clientMessages :: TChan Message,
    clientID :: Maybe Int,
    alivePingTime :: Float,
    list_OtherPlayer :: Map Int OtherPlayer,
    list_key :: Set Char,
    position :: (Float, Float),
    list_Tile :: Map String Picture
    }

data OtherPlayer = OP {
    x :: Maybe Float,
    y :: Maybe Float,
    health :: (Float, Float)
    } deriving (Show)


main :: IO ()
main = do

    -- ToDo:
    -- Aufpassen: TChan eigentlich gedacht für unsafePerformIO
    -- https://stackoverflow.com/questions/10529284/is-there-ever-a-good-reason-to-use-unsafeperformio
    serverMessages <- atomically newTChan :: IO (TChan Message)
    clientMessages <- atomically newTChan :: IO (TChan Message)

    playerTile <- loadBMP "rechteck_gruen.bmp"
    other_Player_Tile <- loadBMP  "rechteck_rot.bmp"

    let initialState = GS
            serverMessages
            clientMessages
            Nothing
            0
            (M.empty :: Map Int OtherPlayer)
            (S.empty :: Set Char)
            (0,0)
            (M.fromList [("player", playerTile), ("op", other_Player_Tile)])

    forkIO $ runTCPClient "127.0.0.1" "3000" $ \socket -> do
        void $ forkIO $ forever $ do
            msg <- recv socket 1024
            --print $ "test" ++ show (fromByteString  msg)
            atomically $ writeTChan serverMessages $ fromByteString msg

        void $ forever $ do
            msg <- atomically $ readTChan clientMessages
            --print msg
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
    -- ToDo: code hier ist sehr!!!!!!!!!! hacki
    let playerTile = M.lookup "player" $ list_Tile gs

        otherTile = M.lookup "op" $ list_Tile gs


        l_op = M.elems $ list_OtherPlayer gs



        t = Prelude.map (\op -> translate 0 0 $ (\(Just p)  -> p)  otherTile) l_op
        tt =  Prelude.map (\op -> let
                                        foo = (x op, y op)
                                        test (Just x, Just y) = translate x y $ (\(Just p)  -> p)  otherTile
                                        test _ = translate 50 50  $ (\(Just p)  -> p)  otherTile

                                        (min, max) = health op
                                        {-
                                        max = 100
                                        1 = 100/max
                                        
                                        
                                        -}

                                        healthBar (Just x, Just y) =  translate (x - 16) (y + 20)  (lineLoop [(0,0),(32/max * min,0)])
                                        healthBar _ = Blank

                                            in [test foo, healthBar foo]) l_op


        trans_player = uncurry translate (position gs) $ (\(Just p) -> p) playerTile
    --mapM_ print l_op
    --print $ length l_tile
    return $ pictures $ trans_player : concat tt --l_tile


handleInput :: Event -> GameState -> IO GameState
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

        _handleServerMessages :: GameState -> IO GameState
        _handleServerMessages gs = do
            mServerMessage <- atomically $ tryReadTChan $ serverMessages gs
            if isNothing mServerMessage
                then return gs
                else do
                    let isID (Just (Message _ (SetID _))) = True
                        isID _ = False
                    if      isNothing (clientID gs) 
                        &&  not ( isID mServerMessage) 
                        then return gs
                        else do
                            case (\(Just msg) -> msg) mServerMessage of
                                -- hier müsste verm. auch ein map handling stattfinden
                                Message [Source Server] (SetID i) -> do
                                    -- unter umständen muss respnse gestored werden wenn server antwort zu schnell kommt
                                    atomically $ writeTChan (clientMessages gs) $ Message [Source (Client i)] GetMyInfo 
                                    return $ gs {clientID = Just i} 

                                Message [Source (Client i)] (PositionUpdate (x,y)) -> do
                                    let updatedPlayer = case M.lookup i $ list_OtherPlayer gs of
                                                    Just op -> op {x = Just x, y = Just y}
                                                    Nothing -> OP {x = Just x, y = Just y, health = (0,0)}
                              
                                    return gs {list_OtherPlayer = M.insert i updatedPlayer $ list_OtherPlayer gs}
                                    
                                Message [Source Server] (PlayerInformation []) -> do
                                    -- ... ToDo
                                    return gs
                                _ -> return gs







        {--
        --depricated -> zukünftig ersetzten mit: _handleServerMessages
        handleServerMessages :: GameState -> IO GameState
        handleServerMessages gs = do
            mServerMessage <- atomically $ tryReadTChan  $ serverMessages gs
            case mServerMessage of
                Just (Message [Source Server] (SetID i)) -> do
                    --print mServerMessage
                    atomically $ writeTChan (clientMessages gs) $ Message [Source $ Client i, Target Broadcast ] Introduce
                    return $ gs {clientID = Just i}

                -- ToDo: auslagern -------------------------
                -- überlegen ob redundante Abfragen besser gelöst werden können
                Just (Message [Source (Client i)] Introduce ) -> do
                    --print mServerMessage
                    if isNothing $ clientID gs
                        then return gs
                        else if (\(Just i) -> i) (clientID gs) == i
                            then return gs
                            else do
                                let newUnit = OP {x = Nothing, y = Nothing, health = (100,100)}
                                return gs {list_OtherPlayer = M.insert i newUnit $ list_OtherPlayer gs}
                Just (Message [Source (Client i)] (PositionUpdate (_x, _y))) -> do
                    --print mServerMessage
                    if isNothing $ clientID gs
                        then return gs
                        else if (\(Just i) -> i) (clientID gs) == i
                            then return gs
                            else do
                                --print mServerMessage
                                let -- makeMaybe (x,y) = (Just x, Just y)
                                    updatedOP = case M.lookup i $ list_OtherPlayer gs of
                                                    Just op -> op {x = Just _x, y = Just _y}
                                                    Nothing -> OP {x = Just _x, y = Just _y, health = (0,0)}
                                --print $ makeMaybe p
                                --return gs
                                --print ( M.lookup i $ list_OtherPlayer gs  :: Maybe OtherPlayer)
                                return gs {list_OtherPlayer = M.insert i updatedOP $ list_OtherPlayer gs}
                --Just (Message [Source (Client c), Target (Client b)] (Action i)) -> return gs
                _ -> do
                    unless (isNothing mServerMessage) $ print mServerMessage
                    return gs
        --}
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
        >=>     _handleServerMessages
        >=>     procedureInput
        ) _gameState


