module DataStructures where
import Data.Map as Map

-- each player has list of QuestInfo, representing the quests he has currently "assigned" to him
data QuestInfo = QuestInfo
    { index :: Int
    , contractor :: String
    , quest :: Quest
    , state :: QuestState
    }
    deriving(Show)

data Quest = Quest 
    { qtype :: QuestType
    , progress :: QuestProgress
    , reward :: QuestReward
    , dialogue :: Dialogue
    }
    deriving(Show)

data QuestType 
    = Spend
    | Fetch
    | Kill
    | Discover
    | Collect
    deriving(Show, Eq)

data QuestState
    = OnGoing
    | Done
    | RewardReceived
    deriving(Show, Eq)

data QuestProgress
    = Counter { current :: Int, target :: Int}
    | CountAndCond { condition :: Bool, current :: Int, target :: Int}
    | Flag Bool
    deriving(Show)

data QuestReward = Item | StatPoint Int deriving Show

data Action = BeginQuestline | Accept | MarkAsDone | KeepGoing deriving Show

type ActiveQuests = [QuestInfo]

data Item = Equipment | Weapon | QuestItem | Pebble

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

-- data Response = Option1 String | Option2 String deriving Show
type Response = String