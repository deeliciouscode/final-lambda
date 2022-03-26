{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}


module Message (Message (..), decodeMessage, encodeMessage, Player (..), MessagePayload (..), emptyMessage) where

import GHC.Generics (Generic)

import Data.Binary
import Control.Monad (liftM2, liftM, liftM3)
import Data.ByteString.Lazy.Internal
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy (fromStrict, toStrict)
import Language.Haskell.TH
import Data.Char (toLower)




-- https://stackoverflow.com/questions/71242310/encode-custom-data-type-to-lazy-byte-string/71242829?noredirect=1#
data Message = M {destination :: Player, source :: Player, payload :: MessagePayload} 
    deriving stock Generic 
    deriving anyclass Binary
    deriving Show

    -- ToDo: Damage "Int" -> doppelt gemoppelt da in Message playerID schon gegeben
data MessagePayload = COORD (Float,Float) | ABILITY String |  DAMAGE Int Float | ID | EMPTY -- "message payload"
    deriving stock Generic 
    deriving anyclass Binary
    deriving Show
    deriving Eq
data Player = BroadCast | PlayerID Int -- PlayerID
    deriving stock Generic 
    deriving anyclass Binary
    deriving Show
    deriving Eq


emptyMessage :: Message
emptyMessage = M BroadCast BroadCast EMPTY

--depricated
--funktioniert aber zu statisch -.-
{-
instance Binary Player where
    get = do
        tag <- getWord8
        case tag of
            0 -> pure NotSet
            1 -> fmap Set get

    put NotSet      = put (0 :: Word8 )
    put (Set i)     = do
        put (1 :: Word8)
        put i

instance Binary MessagePayload where
    get             = do    tag <- getWord8
                            case tag of
                                0 -> fmap COORD get
                                1 -> fmap ABILITY get
                                2 -> pure EMPTY

    put (COORD p)   = do    put (0 :: Word8 )
                            put p
    put (ABILITY p) = do    put (1 :: Word8 )
                            put p
    put _           = do    put (2 :: Word8 )

instance Binary Message where
    get = liftM2  M get get
    put b = do
        put (playerID b)
        put (payload b)
        --put (_payload b)

-}



--decodeMessage :: ByteString -> Message
decodeMessage :: C.ByteString -> Message
decodeMessage bS = decode $ fromStrict bS

encodeMessage :: Message -> C.ByteString
encodeMessage msg = toStrict $ encode msg



