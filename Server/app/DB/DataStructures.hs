{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module DB.DataStructures where
import Database.PostgreSQL.Simple
import GHC.Generics

data ContractorDB = ContractorDB 
    { id:: Int
    , name :: String
    , ql :: Int
    } 
    deriving (Show, Generic, FromRow)

data QuestlineDB = QuestlineDB 
    { id_:: Int
    , q_id :: Int
    , ql_id :: Int
    } 
    deriving (Show, Generic, FromRow)

data ItemDB = ItemDB 
    { i_id :: Int
    , type_ :: Int 
    } 
    deriving (Show, Generic, FromRow)

data ItemTypeDB = ItemTypeDB 
    { it_id :: Int
    , desc :: String 
    } 
    deriving (Show, Generic, FromRow)

data QuestDB = QuestDB 
    { qu_id :: Int
    , qu_type :: Int 
    } 
    deriving (Show, Generic, FromRow)

data TypeDB = TypeDB 
    {qt_id :: Int
    , qt_type :: String
    } 
    deriving (Show, Generic, FromRow)

data ProgressDB = ProgressDB 
    {p_id :: Int
    , qp_id :: Int
    , cur :: Int
    , tar :: Int
    , survived :: Bool
    , cond :: Bool
    } 
    deriving (Show, Generic, FromRow)

data DialogueDB = DialogueDB 
    {d_id :: Int
    , qd_id :: Int
    , prop :: String
    , option1 :: String
    , option2 :: String
    , sendoff :: String
    } 
    deriving (Show, Generic, FromRow)

data RewardDB = RewardDB 
    {r_id :: Int
    , qr_id :: Int
    , ri_id :: Int
    , statPoint :: Int
    } 
    deriving (Show, Generic, FromRow)

data QuestInfoDB = QuestInfoDB 
    {pl_id :: Int
    , qi_id :: Int
    , c_id :: Int
    , currq_id :: Int
    , state_id :: Int
    } 
    deriving (Show, Generic, FromRow)

data QuestStateDB = QuestStateDB 
    {qs_id :: Int
    , qstate :: String
    } 
    deriving (Show, Generic, FromRow)