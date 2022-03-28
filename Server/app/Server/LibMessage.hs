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

data SerialiseTest = 
        L [Int] 
    |   R {val1 :: Int, val2 :: [Int]}
    deriving stock (Generic)
    deriving anyclass (Serialise)
    deriving Show
{-
instance Serialise SerialiseTest where
    encode = encodeTest
    decode = decodeTest

encodeTest :: SerialiseTest -> Encoding
encodeTest (L list) = encodeListLen 2 <> encodeWord 0 <> Codec.Serialise.encode list
encodeTest (R v1 v2) = encodeListLen 3 <> encodeWord 1 <> Codec.Serialise.encode v1 <> Codec.Serialise.encode v2


decodeTest :: Decoder s SerialiseTest
decodeTest = do
    len <- decodeListLen
    tag <- decodeWord

    --L <$>Codec.Serialise.decode 
    case (len, tag) of
      (2, 0) -> L <$> Codec.Serialise.decode 
      (3, 1) -> R <$> Codec.Serialise.decode <*> Codec.Serialise.decode
      _      -> fail "invalid Type encoding" 
-}

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
                |   MapBotPosition (Map Int (Float, Float))         -- Map from Bot ID to Bot position
                |   Action Int                                      -- ActionID
                |   HealthChanged Float                             -- wenn positiv: dann Heal
                                                                    -- wenn negativ: damage
                |   EffectGained Int                                -- Effect ID
                |   EffectWanished Int                              -- Effect ID
                |   M String                                        -- Map ID + player update?                               -- 
                |   PlayerInformation (Map Int PlayerInfo)
                |   GetMyInfo                                       -- Ruft stored PlayerInfo aus DataBase ab
                                                                    -- ebenfalls trigger für PlayerInformation
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