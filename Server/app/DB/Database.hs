-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module DB.Database where
import Database.PostgreSQL.Simple
import GHC.Int
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)

import qualified Data.Text as T
import Control.Monad
import Database.PostgreSQL.Simple.ToField
import Codec.Picture.Metadata (Value(String))
import Quests.DataStructures
-- import Quests.DataStructures (QuestInfo(contractor))





main :: IO Contractor 
main = do
  print "Hi"
  a <- getContractorById 1
  return $ emptyC a
  -- getUserByName "Paul"
    -- con <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=fun4Sql"
    -- [Only i] <- query_ con "select * from accounts"
    -- return i
    -- putStrLn "String"
    -- con <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=fun4Sql"
    -- line <- quickQuery' con "select * from accounts" [] 
    -- mapM_ print line
    -- disconnect con

connectionString = "host=localhost dbname=postgres user=postgres password=fun4Sql"

getConnection = connect defaultConnectInfo

-- select = do
--   conn <- connectPostgreSQL connectionString
--   [Only i] <- query_ conn "select * from user_" 
--   return i

emptyC :: C -> Contractor
-- emptyC C {DB.Database.id, name, ql} = Contractor id name ql
emptyC C {DB.Database.id, DB.Database.name, ql} = Contractor id name []


test :: IO Int64
test = do
  connection <- connect defaultConnectInfo
  execute connection "insert into user_ (name, age) values (?,?)" ("David" :: String, 21 :: Int)


getUserHardcoded = do
  connection <- connect defaultConnectInfo
  [Only i] <- query_ connection "select name from user_ where name = 'David'" --[id :: Int]
  return (i::String)

getUserNameByName name = do
  connection <- connect defaultConnectInfo
  [Only i] <- query connection "select name from user_ where name = (?)" [name :: String]
  return (i::String)


getUserByName name = do
  connection <- connect defaultConnectInfo
  user <- query connection "select * from user_ where name = (?)" [name :: String]
  forM_ user $ \(name,age) ->
    print $ User (T.unpack name) (age :: Int)

getContractorById :: Int -> IO C
getContractorById id = do
  connection <- connect defaultConnectInfo
  c <- query connection "select * from contractor where id = (?)" [id]
  return $ head c


getQuestlineByContractor c_id = do
  connection <- connect defaultConnectInfo
  query connection "select * from questline where ql_id = (?)" [c_id]




-- getQuestlineById



-------------------------------adapter types-------------------------------------

data C = C { id:: Int, name :: String, ql :: Int} deriving (Show, Generic, FromRow)
data User = User { u_name :: String, age :: Int } deriving (Show, Generic, FromRow)
-- getContractorById :: IO ()
-- getContractorById = do 
--   connection <- connect defaultConnectInfo
--   -- [Only i] <- query "select * from contractor where id = ?" (id :: Int)
--   -- return i
--   -- xs <- query_ connection "select name,age from user_ where name = 'Paul'"
--   xs <- query connection "select name,age from user_ where name = (?)" ["Paul"]
--   forM_ xs $ \(name,age) ->
--     print $ User (T.unpack name) (age :: Int)
    -- putStrLn $ User (T.unpack name) ++ " is " ++ show (age :: Int)


-- getUserNameByName = do 
--   connection <- connect defaultConnectInfo
--   [Only i] <- query_ connection "select name from user_ where name = 'David'" --[id :: Int]
--   return (i::String)
  -- xs <- query connection $ "select name,age from user_ where name in (?)" $ 
  --   Only $ In ["Paul", "Test"]
  -- forM_ xs $ \(name,age) ->
  --   print $ User (T.unpack name) (age :: Int)
    -- putStrLn $ User (T.unpack name) ++ " is " ++ show (age :: Int)



-- instance FromRow User where
--     fromRow = User <$> field <*> field

-- create = do
--     connection <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=fun4Sql"
--     execute_ connection "create table test"

-- executeQuerry :: (String, [SqlValue]) -> IO [[SqlValue]]
-- executeQuerry querry = do
--     conn <- connectPostgreSQL "String"
--     result <- uncurry (quickQuery conn) querry 
--     disconnect conn
--     return result

-- insertTest = do
--     connection <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=fun4Sql"
    -- executeRaw $ Statement "insert into accounts values (2, 'test')"
    -- runRaw connection "insert into accounts values (2, 'test')"
    -- executeRaw Statement <- "insert into accounts values (2, 'test')"

    -- commit connection
    -- disconnect connection

-- insertTest2 a b = do
--     connection <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=fun4Sql"
--     -- executeRaw $ Statement "insert into accounts values (2, 'test')"
--     run connection "insert into accounts values (?, ?)" [toSql a, toSql b]
--     -- executeRaw Statement <- "insert into accounts values (2, 'test')"

--     commit connection
--     disconnect connection

-- insertTest3 User {userId, username} = do
--     connection <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=fun4Sql"
--     run connection "insert into accounts values (?, ?)" [toSql userId, toSql username]

--     commit connection
--     disconnect connection


-- setupTest c@connection sql = do
--     runRaw c sql
--     commit c
--     disconnect c

-- createTable = "create"

-- testConnection = "host=localhost dbname=postgres user=postgres password=fun4Sql"
-- user1 = User 4 "Tim"

-- insertStateTest 


----------------------------------------------------------------------------------