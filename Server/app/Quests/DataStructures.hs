{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
module Quests.DataStructures where
import Data.Map as Map
import Data.Binary ( Binary(put, get), decode, encode )
import GHC.Generics (Generic)

-- each player has list of QuestInfo, representing the quests he has currently "assigned" to him
data QuestInfo = QuestInfo
    { index :: Int
    , contractor :: String
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

data Item = Equipment | Weapon | QuestItem
    deriving Show
    deriving Eq
    deriving stock Generic
    deriving anyclass Binary

-- type StatPoint = Int

data Contractor = Contractor
    { name :: String
    , questline :: Questline
    }

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
