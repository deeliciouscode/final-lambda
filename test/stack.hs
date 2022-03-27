import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Control.Monad (forM_, forever)
import Control.Monad.Trans.Reader (Reader, ask, ReaderT (runReaderT))
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM (newTChan, atomically)
import Control.Concurrent (newChan, forkIO, forkFinally, Chan, readChan, writeChan)

--[[SQLInt Value, SQL Value]] 




foo2 = do

    con <- connectPostgreSQL "host=localhost dbname=MyDataBase user=postgres password=fun4Sql"
    line <- quickQuery' con "select * from fifa" [] 
    mapM_ print line
    
    disconnect con
    putStrLn ""


foo str1 str2 = connectPostgreSQL str1 >>= \x -> quickQuery' x str2 []

executeQuerry :: (String, [SqlValue]) -> IO [[SqlValue]]
executeQuerry querry = do
    conn <- connectPostgreSQL "String"
    result <- uncurry (quickQuery conn) querry 
    disconnect conn
    return result



dataBase = do
    querryQ <- newChan :: IO (Chan String)
    outputQ <- newChan :: IO (Chan [[SqlValue]])
    conn <- connectPostgreSQL "host=localhost dbname=MyDataBase user=postgres password=fun4Sql"

    
    
    forkFinally (runReaderT (
            forever $ do
                querry <- lift $ readChan querryQ
                ask >>= \c -> lift $ quickQuery' c querry [] >>= writeChan outputQ 
        )conn) (const $ disconnect conn)                                                    

    forkIO $ forever $ do
        readChan outputQ >>= print

    forever $ do
        input <- getLine
        writeChan querryQ input
    
f = do

    conn <- connectPostgreSQL ""
    let a = undefined 
    run conn "" a