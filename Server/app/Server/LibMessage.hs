{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Server.LibMessage where

import Network.Socket(Socket)
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString  as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import GHC.Generics ( Generic )
import Data.Binary ( Binary(put, get, putList), decode, encode, Word8 )

import Data.Map as M ( Map )
import Data.Vector.Storable as VS

import Codec.Serialise
import Codec.Serialise.Encoding (Encoding, encodeListLen, encodeWord)
import Codec.Serialise.Decoding (Decoder, decodeListLen, decodeWord)
import Types
import Quests.Quests
import Quests.DataStructures


data PlayerInfo = PI {
    pI_mapID :: Int, -- -1 als not set
    pI_health :: PointF, -- (-1,-1) als not set
    pI_position :: PointF,
    pI_direction :: PointF,
    pI_velocity :: Float
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
                |   PositionUpdate PointF
                |   MapBotPosition (Map Int PointF)                 -- Map from Bot ID to Bot position
                |   Action Int                                      -- ActionID
                |   HealthChanged Float                             -- wenn positiv: dann Heal
                                                                    -- wenn negativ: damage
                |   EffectGained Int                                -- Effect ID
                |   EffectWanished Int                              -- Effect ID
                |   M String                                        -- Map ID + player update?                               -- 
                |   PlayerInformation (Map Int PlayerInfo)
                |   GetMyInfo                                       -- Ruft stored PlayerInfo aus DataBase ab
                                                                    -- ebenfalls trigger f??r PlayerInformation
                |   Null
                |   MapVector (VS.Vector Int)
                |   WrapList [Int]
    deriving stock Generic
    deriving anyclass Binary
    deriving Show
    deriving Eq


instance (Storable a, Binary a) => Binary (VS.Vector a) where
    put  = putList . toList
    get = fromList <$> get



fromByteString :: ByteString -> Message
fromByteString bS = Data.Binary.decode $ fromStrict bS

toByteString :: Message -> ByteString
toByteString msg = toStrict $ Data.Binary.encode msg