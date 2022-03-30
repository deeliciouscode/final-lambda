{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module DB.Database where
import Database.PostgreSQL.Simple
import GHC.Int
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)

import qualified Data.Text as T
import Control.Monad
import Database.PostgreSQL.Simple.ToField
import Codec.Picture.Metadata (Value(String), insert)
import Quests.DataStructures
import Quests.Quests
import DB.DataStructures


main :: IO ()
main = do
  print "Hi"

getConnection :: IO Connection
getConnection = connect defaultConnectInfo

getQuestInfoDB :: Int -> IO [DB.DataStructures.QuestInfoDB]
getQuestInfoDB i = do
  connection <- connect defaultConnectInfo
  query connection "select * from quest_info where player_id = (?)" [i]

getQuestStateDB :: Int -> IO [DB.DataStructures.QuestStateDB]
getQuestStateDB i = do
  connection <- connect defaultConnectInfo
  query connection "select * from quest_state where id = (?)" [i]

getContractorById :: Int -> IO ContractorDB
getContractorById id = do
  connection <- connect defaultConnectInfo
  c <- query connection "select * from contractor where id = (?)" [id]
  return $ head c

getQuestlineByContractorId :: Int -> IO [QuestlineDB]
getQuestlineByContractorId c_id = do
  connection <- connect defaultConnectInfo
  query connection "select * from questline where ql_id = (?)" [c_id]

getItem :: Int -> IO [DB.DataStructures.ItemDB]
getItem i_id = do
  connection <- getConnection
  query connection "select * from item where id = (?)" [i_id]

getItemType :: Int -> IO [DB.DataStructures.ItemTypeDB]
getItemType it_id = do
  connection <- getConnection
  query connection "select * from item_type where id = (?)" [it_id]

getQuest :: Int -> IO [DB.DataStructures.QuestDB]
getQuest q_id = do
  connection <- getConnection
  query connection "select * from quest where id = (?)" [q_id]

getQuestTypeDB :: Int -> IO [DB.DataStructures.TypeDB]
getQuestTypeDB qt_id = do
  connection <- connect defaultConnectInfo
  query connection "select * from quest_type where id = (?)" [qt_id :: Int]
getProgress :: Int -> IO [DB.DataStructures.ProgressDB]
getProgress p_id = do
  connection <- getConnection
  query connection "select * from progress where id = (?)" [p_id]

getProgressByQuest :: Int -> IO [DB.DataStructures.ProgressDB]
getProgressByQuest q_id = do
  connection <- getConnection
  query connection "select * from progress where q_id = (?)" [q_id]

getDialogue :: Int -> IO [DB.DataStructures.DialogueDB]
getDialogue p_id = do
  connection <- getConnection
  query connection "select * from dialogue where id = (?)" [p_id]

getDialogueByQuest :: Int -> IO [DB.DataStructures.DialogueDB]
getDialogueByQuest q_id = do
  connection <- getConnection
  query connection "select * from dialogue where q_id = (?)" [q_id]

getReward :: Int -> IO [DB.DataStructures.RewardDB]
getReward r_id = do
  connection <- getConnection
  query connection "select * from reward where id = (?)" [r_id]

getRewardByQuest :: Int -> IO [DB.DataStructures.RewardDB]
getRewardByQuest q_id = do
  connection <- getConnection
  query connection "select * from reward where q_id = (?)" [q_id]

getItemIdFromReward :: [RewardDB] -> Int
getItemIdFromReward (RewardDB {r_id, qr_id, ri_id, statPoint}:xs) = ri_id
getItemIdFromReward [] = -1

getItemTypeFromItem :: ItemDB -> Int
getItemTypeFromItem ItemDB { i_id, type_ } = type_

getQuestType :: [QuestDB] -> Int
getQuestType (QuestDB {qu_id, qu_type}:xs) = qu_type
getQuestType [] = -1

getQuestDesc :: [TypeDB] -> String
getQuestDesc (TypeDB {qt_id, qt_type}:xs) = qt_type
getQuestDesc [] = ""

getRewardItemId :: [RewardDB] -> Int
getRewardItemId (RewardDB {r_id, qr_id, ri_id, statPoint}:xs) = ri_id
getRewardItemId [] = -1

getQuestInfoContractor :: [QuestInfoDB] -> Int
getQuestInfoContractor (QuestInfoDB {pl_id, qi_id, c_id, currq_id, state_id}:xs) = c_id
getQuestInfoContractor [] = -1

createDialogue :: [DialogueDB] -> Dialogue
createDialogue [DialogueDB {d_id, qd_id, prop, option1, option2, sendoff}] = Dialogue prop option1 sendoff

createQuestState :: [QuestStateDB] -> QuestState
createQuestState (QuestStateDB {qs_id, qstate}:xs) =
  case qstate of
    "Done" -> Done
    "OnGoing" -> OnGoing
    "RewardReceived" -> RewardReceived

createQuestType :: String -> QuestType
createQuestType qt =
  case qt of
    "Collect" -> Collect
    "Spend" -> Spend
    "Fetch" -> Fetch
    "Discover" -> Discover
    "Kill" -> Kill


progressComb :: [ProgressDB] -> QuestProgress
progressComb (ProgressDB {p_id, qp_id, cur, tar, survived, cond}:xs) = CountAndCond survived cur tar
progressCounter :: [ProgressDB] -> QuestProgress
progressCounter(ProgressDB {p_id, qp_id, cur, tar, survived, cond}:xs) = Quests.DataStructures.Counter cur tar
progressBool :: [ProgressDB] -> QuestProgress
progressBool (ProgressDB {p_id, qp_id, cur, tar, survived, cond}:xs) = Flag cond
progressBoolComb :: [ProgressDB] -> QuestProgress
progressBoolComb (ProgressDB {p_id, qp_id, cur, tar, survived, cond}:xs) = BoolComb survived cond

createQuestProgress :: QuestType -> [ProgressDB] -> QuestProgress
createQuestProgress qt p =
  case qt of
      Kill -> progressComb p
      Spend -> progressCounter p
      Fetch -> progressBool p
      Discover -> progressBool p
      Collect -> progressCounter p

createQuestReward :: [RewardDB] -> QuestReward
createQuestReward [] = StatPoint 0
createQuestReward (RewardDB {r_id, qr_id, ri_id, statPoint}:xs) = 
  if statPoint > 0
    then StatPoint statPoint
    else Item

createQuest :: Int -> IO Quest
createQuest questId = do
  quest <- DB.Database.getQuest questId
  let qt_id = getQuestType quest
  questType <- getQuestTypeDB qt_id
  let qt_desc = getQuestDesc questType
  progress <- getProgressByQuest questId
  reward <- getRewardByQuest questId
  dialogue <- getDialogueByQuest questId
  let item_id = getRewardItemId reward
  item_type <- getItemType item_id
  return $ Quest (createQuestType qt_desc) (createQuestProgress (createQuestType qt_desc) progress) (createQuestReward reward) $ createDialogue dialogue

createQuestInfo :: Int -> IO QuestInfo
createQuestInfo i = do
  quest_info <- getQuestInfoDB i
  let qi = getFromQi quest_info
  dialogue <- getDialogueByQuest $ frt qi
  quest <- createQuest $ frt qi
  qstate <- getQuestStateDB $ fth qi
  return $ QuestInfo (fst' qi) (snd' qi) (trd qi) quest $ createQuestState qstate


fth :: (Int, Int, Int, Int, Int) -> Int 
fth (p_id, qi_id, c_id, currq_id, state_id) = state_id

frt :: (Int, Int, Int, Int, Int) -> Int 
frt (p_id, qi_id, c_id, currq_id, state_id) = currq_id

trd :: (Int, Int, Int, Int, Int) -> Int 
trd (p_id, qi_id, c_id, currq_id, state_id) = c_id

snd' :: (Int, Int, Int, Int, Int) -> Int 
snd' (p_id, qi_id, c_id, currq_id, state_id) = qi_id

fst' :: (Int, Int, Int, Int, Int) -> Int 
fst' (p_id, qi_id, c_id, currq_id, state_id) = p_id

getFromQi :: [QuestInfoDB] -> (Int, Int, Int, Int, Int)
getFromQi (QuestInfoDB {pl_id, qi_id, c_id, currq_id, state_id}:xs) = (pl_id, qi_id, c_id, currq_id, state_id)

getQuestIdFromInfo :: [QuestInfoDB] -> Int
getQuestIdFromInfo (QuestInfoDB {pl_id, qi_id, c_id, currq_id, state_id}:xs) = currq_id
getQuestIdFromInfo [] = -1

insertQuestType id type_ = do
  connection <- getConnection
  execute connection  "insert into quest_type (id, type_) values (?,?)" (id,type_)

updateQuestType id type_ = do
  connection <- getConnection
  execute_ connection  "update quest_type set id = qt.id from (values (?,?)) as qt(id,type_)"
  -- execute connection  "update quest_type (id, type_) values (?,?)" (id,type_)

insertQuestState id type_ = do
  connection <- getConnection
  execute connection  "insert into quest_state (id, type_) values (?,?)" (id,type_)

insertItemType id type_ = do
  connection <- getConnection
  execute connection  "insert into item_type (id, type_) values (?,?)" (id,type_)

insertContractor id name = do
  connection <- getConnection
  execute connection  "insert into contractor (id, type_) values (?,?)" (id,name)

insertQuest id name = do
  connection <- getConnection
  execute connection  "insert into quest (id, q_type) values (?,?)" (id,name)

insertQuestLine id q_id ql_id = do
  connection <- getConnection
  execute connection  "insert into questline (id, q_id, ql_id) values (?,?,?)" (id, q_id, ql_id)

insertItem id i_type = do
  connection <- getConnection
  execute connection  "insert into item (id, q_type) values (?,?)" (id,i_type)

insertReward id q_id item stat = do
  connection <- getConnection
  execute connection  "insert into reward (id, q_id item, stat_point) values (?,?,?)" (id, q_id item, stat)

insertProgress id q_id cur tar surv cond = do
  connection <- getConnection
  execute connection  "insert into progress (id, q_id, i_id) values (?,?,?)" (id, q_id i_id)

test id q_id cur tar surv cond = do
  connection <- getConnection
  execute connection  "insert into progress (id, q_id, i_id) values (?,?,?)" (id, q_id i_id)

insertDialogue id q_id prop opt1 opt2 end = do
  connection <- getConnection
  execute connection  "insert into dialogue (id, q_id, proposition, option1, option2, sendoff) values (?,?)" (id, q_id i_id, prop, opt1, opt2, end)

insertQuestInfo id p_id c_id q_id state = do
  connection <- getConnection
  execute connection  "insert into quest_info (id, player_id, c_id, quest, state) values (?,?,?,?,?)" (id, p_id, c_id, q_id, state)

-- comment out to avoid crashing
-- updateQuestInfo QuestInfo {playerId, index, contractor, quest, state} = do
--   connection <- getConnection
--   quest_info <- getQuestInfoDB playerId
--   execute connection  "update quest_info set (player_id, index, c_id, quest, state) values (?,?,?,?,?) where playerId = playerID" (playerId, index, contractor, q_id, state)