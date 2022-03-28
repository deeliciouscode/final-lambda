{-# LANGUAGE NamedFieldPuns #-}
module Quests.Quests where
import Quests.DataStructures
import Data.Maybe
import Data.Map as Map

interactWithRewardGiverPersonBetterNameHere :: ActiveQuests -> [QuestReward]
interactWithRewardGiverPersonBetterNameHere pq = 
    let quests = Prelude.map getQuest $ Prelude.filter isQuestFinished pq
    in Prelude.map getReward quests

isQuestFinished :: QuestInfo -> Bool
isQuestFinished QuestInfo {index, contractor, quest, state} = state == Done



--TODO how should this conversation be handeld?
interactWithContractor :: ActiveQuests -> String -> Questline -> ActiveQuests
interactWithContractor pq c ql = 
    let action = getAvailableAction c pq
        currentPlayerQuest = getQuestInfoByContractor pq c
        otherQuests = filterQuests pq c
        dialogue = getDialogue currentPlayerQuest
        isKnown = hasQuestFromContractor pq c
    in if isJust currentPlayerQuest
        then case action of
            KeepGoing -> pq
            BeginQuestline -> acceptQuest pq 1 c $ fromJust $ getNextFromQl ql 0
            Accept -> progressQuestline pq c ql
            MarkAsDone -> setQuestDone (fromJust currentPlayerQuest) : otherQuests -- TODO: how can reward be dished out????
        else pq

-- interactWithContractor :: ActiveQuests -> String -> Questline -> IO ActiveQuests
-- interactWithContractor pq c ql = 
--     let action = getAvailableAction c pq
--         currentPlayerQuest = getQuestInfoByContractor pq c
--         otherQuests = filterQuests pq c
--         dialogue = getDialogue currentPlayerQuest
--     in if isJust currentPlayerQuest
--         then case action of
--             KeepGoing -> placeholder dialogue pq
--             BeginQuestline -> placeholder dialogue $ acceptQuest pq 1 c $ fromJust $ getNextFromQl ql 0
--             Accept -> placeholder dialogue $ progressQuestline pq c ql
--             MarkAsDone ->  placeholder dialogue $ setQuestDone (fromJust currentPlayerQuest) : otherQuests -- TODO: how can reward be dished out????
--         else placeholder ("Keep", "On") pq

-- todo: look at formjust
getDialogue :: Maybe QuestInfo -> (String, String)
getDialogue qi = helper $ getQuest $ fromJust qi

helper Quest{qtype, progress, reward, dialogue} = helper' dialogue

helper' Dialogue {proposition, response, end} = (proposition, end)

-- the contractor should evaluate if the player has no quest from him, is in the works of completing a quest for him or is eligable for a new quest
getAvailableAction :: String -> ActiveQuests -> Action
-- getAvailableAction c [] = BeginQuestline
getAvailableAction c pq@(QuestInfo {index, contractor, quest, state}:xs) = 
    if hasQuestFromContractor pq c
        then getAvailableAction' pq c
        else BeginQuestline

getAvailableAction' :: ActiveQuests -> String -> Action
getAvailableAction' pq c = 
    let quest = fromJust $ getQuestByContractor pq c
    in if isQuestDone quest
        then MarkAsDone
        else KeepGoing

hasQuestFromContractor :: ActiveQuests -> String -> Bool
hasQuestFromContractor [] c = False
hasQuestFromContractor (QuestInfo {index, contractor, quest, state}:xs) c = 
    contractor == c || hasQuestFromContractor xs c
---------------------------------------------------------------------------------------

-- replace the players current quest (in regards to the given contractor) with the next one in the questline
progressQuestline :: ActiveQuests -> String -> Questline -> ActiveQuests
progressQuestline pq c ql = 
    let index = getQuestIndex pq c
        nextQuest = getNextFromQl ql index
    in if isJust nextQuest
        then acceptQuest pq (index+1) c $ fromJust nextQuest
        else filterQuests pq c

getNextFromQl :: Questline -> Int -> Maybe Quest
getNextFromQl ql i = 
    let questMap = qlToQlMap ql
        index = i + 1
    in Map.lookup index questMap

getQuestIndex :: ActiveQuests -> String -> Int
getQuestIndex pq@(qi@QuestInfo {index, contractor, quest}:xs) c =
    if contractor == c
        then index
        else getQuestIndex xs c


-- get list of current quests; replace the quest of given contractor with the quest passed to this function in the players currently held quests
acceptQuest :: ActiveQuests -> Int -> String -> Quest -> ActiveQuests
acceptQuest pq i c q = addQuest (filterQuests pq c) i c q

-- accept quest helper
addQuest :: ActiveQuests -> Int -> String -> Quest -> ActiveQuests
addQuest pq i c q = QuestInfo {index=i, contractor=c, quest=q, state = OnGoing} : pq

-- accept quest helper TODO: what happens if only one c and match?
filterQuests :: ActiveQuests -> String -> ActiveQuests
filterQuests [] c = []
filterQuests (QuestInfo {index, contractor, quest, state}:xs) c = 
    if contractor /= c 
        then QuestInfo {index, contractor, quest, state} : filterQuests xs c
        else  filterQuests xs c
-----------------------------------------------------------------------

redeemQuest :: ActiveQuests -> String -> Maybe QuestReward
redeemQuest pq@(QuestInfo {index, contractor, quest}:xs) c =
    let quest' = fromJust $ getQuestByContractor pq c 
    in if isQuestDone quest'
        then Just $ getReward quest'
        else Nothing

isRequirementMet :: QuestProgress -> Bool
isRequirementMet (Flag flag) = flag
isRequirementMet (Counter { current, target}) = current >= target
isRequirementMet (CountAndCond { condition, current, target}) = condition && current >= target

isQuestDone :: Quest -> Bool
isQuestDone Quest {qtype, progress, reward} = isRequirementMet progress

getReward :: Quest -> QuestReward
getReward Quest {qtype, progress, reward} = reward

getQuest :: QuestInfo -> Quest
getQuest QuestInfo {index, contractor, quest} = quest

-- no issues due to application flow & lazy eval
getQuestByContractor :: ActiveQuests -> String -> Maybe Quest
getQuestByContractor (QuestInfo {index, contractor, quest}:xs) c = 
    if contractor == c
        then Just quest
        else getQuestByContractor xs c
getQuestByContractor [] c = Nothing

getQuestInfoByContractor :: ActiveQuests -> String -> Maybe QuestInfo
getQuestInfoByContractor (qi@QuestInfo {index, contractor, quest}:xs) c = 
    if contractor == c
        then Just qi
        else getQuestInfoByContractor xs c
getQuestInfoByContractor [] c = Nothing
    
setQuestDone :: QuestInfo -> QuestInfo
setQuestDone QuestInfo {index, contractor, quest, state} = QuestInfo {index, contractor, quest, state = Done}


getQlFromContractor :: Contractor -> Questline
getQlFromContractor Contractor {name, questline} = questline

----------------------- Dirty Helpers ---------------------------

placeholder (a,b) pq = do
    placeholder' (a,b)
    return pq

placeholder' (a,b) = do
    putStrLn a
    reply <- getLine
    putStrLn b

qlToQlMap ql = Map.fromList $ zip [1..] ql

---------------------Test Data-----------------------------------        

quest1 = Quest Collect (Counter 1 2) (StatPoint 1) (Dialogue "Hi" "foo" "Bye")
quest2 = Quest Kill (CountAndCond True 1 2) (StatPoint 1) (Dialogue "Hi" "foo" "Bye")
quest3 = Quest Discover (Flag True) (StatPoint 1) (Dialogue "Hi" "lala" "Bye")

quest4 = Quest Discover (Flag True) (StatPoint 1) (Dialogue "Hi" "foo" "Bye")
quest5 = Quest Discover (Flag True) (StatPoint 1) (Dialogue "Hi" "foo" "Bye")
quest6 = Quest Discover (Flag True) (StatPoint 1) (Dialogue "Hi" "foo" "Bye")

quest7 = Quest Discover (Flag False) (StatPoint 3) (Dialogue "Hi" "lala" "Bye")

testInfo1 = QuestInfo 1 "David" quest1 OnGoing
testInfo2 = QuestInfo 2 "Thomas" quest2 OnGoing
testInfo3 = QuestInfo 3 "Paul" quest3 OnGoing

myQuests = [testInfo1, testInfo2, testInfo3]

testQuestMap = Map.fromList [(1, quest4),(2, quest5),(3,quest6)]

testQuestLine = [quest4, quest5, quest6]