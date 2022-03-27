{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Lib_Message where

import Network.Socket(Socket)
import Data.ByteString.Char8 ( ByteString ) 
import qualified Data.ByteString  as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import GHC.Generics ( Generic ) 
import Data.Binary ( Binary(put, get), decode, encode )

import Data.Map as M ( Map )

data PlayerInfo = PI {
    pI_mapID :: Int, -- -1 als not set
    pI_health :: (Float, Float), -- (-1,-1) als not set
    pI_position :: (Float, Float)
}
    deriving stock Generic 
    deriving anyclass Binary
    deriving Show
    deriving Eq

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





fromByteString :: ByteString -> Message
fromByteString bS = decode $ fromStrict bS

toByteString :: Message -> ByteString
toByteString msg = toStrict $ encode msg