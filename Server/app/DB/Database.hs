-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module DB.Database where
import Database.PostgreSQL.Simple
import GHC.Int


main :: IO GHC.Int.Int64
main = do
    select
    -- con <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=fun4Sql"
    -- [Only i] <- query_ con "select * from accounts"
    -- return i
    -- putStrLn "String"
    -- con <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=fun4Sql"
    -- line <- quickQuery' con "select * from accounts" [] 
    -- mapM_ print line
    -- disconnect con

select = do
  conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=fun4Sql"
  [Only i] <- query_ conn "select * from accounts"
  return i


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

data User = User {userId :: Int, username :: String}

----------------------------------------------------------------------------------