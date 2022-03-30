{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
module Quests.DataStructures where
import Data.Map as Map
import Data.Binary ( Binary(put, get), decode, encode )
import GHC.Generics (Generic)

-- each player has list of QuestInfo, representing the quests he has currently "assigned" to him
data QuestInfo = QuestInfo
    { playerId :: Int
    , index :: Int
    , contractor :: Int
    , quest :: Quest
    , state :: QuestState
    }
    deriving Show
    deriving Eq
    deriving stock Generic
    deriving anyclass Binary

data Quest = Quest 
    { qtype :: QuestType
    , progress :: QuestProgress
    , reward :: QuestReward
    , dialogue :: Dialogue
    }
    deriving Show
    deriving Eq
    deriving stock Generic
    deriving anyclass Binary

data QuestType 
    = Spend
    | Fetch
    | Kill
    | Discover
    | Collect
    deriving Show
    deriving Eq
    deriving stock Generic
    deriving anyclass Binary

data QuestState
    = OnGoing
    | Done
    | RewardReceived
    deriving Show
    deriving Eq
    deriving stock Generic
    deriving anyclass Binary

data QuestProgress
    = Counter { current :: Int, target :: Int}
    | CountAndCond { condition :: Bool, current :: Int, target :: Int}
    | Flag Bool
    | BoolComb Bool Bool
    deriving Show
    deriving Eq
    deriving stock Generic
    deriving anyclass Binary

data QuestReward = Item | StatPoint Int 
    deriving Show
    deriving Eq
    deriving stock Generic
    deriving anyclass Binary

data Action = BeginQuestline | Accept | MarkAsDone | KeepGoing 
    deriving Show
    deriving Eq
    deriving stock Generic
    deriving anyclass Binary

type ActiveQuests = [QuestInfo]

data Item = Equipment Int | Weapon Int | QuestItem Int
    deriving Show
    deriving Eq
    deriving stock Generic
    deriving anyclass Binary

data Contractor = Contractor
    { id :: Int
    , name :: String
    , questline :: Questline
    } 
    deriving Show

type Questline = [Quest]

data Dialogue = Dialogue
    { proposition :: String
    , response :: Response
    , end :: String
    }
    deriving Show
    deriving Eq
    deriving stock Generic
    deriving anyclass Binary

-- data Response = Option1 String | Option2 String deriving Show
type Response = String
