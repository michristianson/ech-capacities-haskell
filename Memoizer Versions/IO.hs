module IO where

import Internals

import System.IO
import System.IO.Unsafe
import Data.Time.Clock.POSIX

import Data.Decimal
import Data.Serialize
import Data.List (inits)
import Data.Maybe (fromJust, isNothing)
import Data.Function (on)

import Database.HDBC
import Database.HDBC.ODBC

logLevel :: Int
logLevel = 80

{- Works: - new connection for each DB interaction (this goes much more slowly than with pool)
          - Use same Statement for query every time!!
 - Doesn't Work: - pool of connections (with any number and expiration time, tryWithResource or withResource)
                 - make new pool for each thm119 call
                 - make new heuristic object for each map of thm119 calls
                 - use semaphore to queue access to a single connection
                 - Making DB functions IO and using unsafePerformIO in getLTs
                 - Making getLTs an IO function
                 - Marking getLTS with NOINLINE pragma
 - Seems to Help: - lower expiration time on and maximum number of pool resources
                  - Marking DB functions with NOINLINE pragma
 - To Try: make thread pool (to test if connections used on multiple threads are bad)
-}

--Useful helper functions {{{
fromRight :: Either a b -> b
fromRight (Left x) = error "fromRight called on Left object"
fromRight (Right x) = x

--Decimal functions
fromDecimal :: (RealFrac t,Read t) => Decimal -> t
fromDecimal d = read (show d)

toDecimal :: (RealFrac t) => t -> Decimal
toDecimal r = let precision = findPrecision r
              in realFracToDecimal precision r
              
roundOff :: (RealFrac t,Read t) => t -> t
roundOff r = fromDecimal (toDecimal r)

findPrecision :: (Integral i,RealFrac t) => t -> i
findPrecision r = head $ dropWhile (\i -> let s = r*10^i in abs (s - fromIntegral (round s)) > 0.001) [0..] 
--}}}
--Logger {{{
timestamp :: String -> IO String
timestamp msg = do
                    time <- round `fmap` getPOSIXTime
                    return (show time ++ " " ++ msg)

writeLevel :: String -> Int -> String
writeLevel msg level
  | level <= 20 = "FATAL ERROR " ++ msg
  | level <= 40 = "ERROR " ++ msg
  | level <= 60 = "WARNING " ++ msg
  | otherwise = "INFO " ++ msg

myLog :: Int -> String -> IO ()
myLog level msg = if level > logLevel then return ()
                  else do
                          toprint <- timestamp $ writeLevel msg level
                          print toprint
                          
logAndDo :: Int -> String -> a -> a
logAndDo level msg expr = unsafePerformIO $ do
                                                myLog level msg
                                                return expr

logFatal = logAndDo 20
logError = logAndDo 40
logWarning = logAndDo 60
logInfo = logAndDo 80

abbrevStr :: String -> String
abbrevStr str = if length str <= 50 then str else (take 47 str) ++ "..."

abbrevListStr :: (Show a) => [a] -> String
abbrevListStr xs = if length (show xs) < 50
                   then show xs
                   else let trunc = last $ takeWhile (\l -> length l < 50) (map show (inits xs))
                        in (take (length trunc - 1) trunc) ++ "...]"
--}}}
--Scan output {{{
--First, a function that converts offsets to their actual value.
removeOffsets :: (RealFrac t1,Read t1,Show t1,RealFrac t2,Read t2,Show t2) =>
                 [(CGen,t1,t2,Maybe Decomposition)] -> (t1 -> t2) -> [(CGen,t1,t2,Maybe Decomposition)]
removeOffsets results midc = map (\(cg,a,off,decomp) -> (cg,a,roundOff (midc a + off),decomp)) results

--This function converts a scan result into a String, which can then be printed to the desired
--output location.
printPlain :: (Show t1,Show t2) => (CGen,t1,t2,Maybe Decomposition) -> String 
printPlain (cg,a,c,result) = (show cg) ++ ", " ++ (show a) ++ ", " ++ (show c) ++ ": " ++ 
                             (if isNothing result then "Nothing" else show (fromJust result))

--The following functions output results to StdOut, a given file, or both (respectively).
scanToStd :: (Show t1,Show t2) => [(CGen,t1,t2,Maybe Decomposition)] -> IO ()
scanToStd results = mapM_ (putStrLn . printPlain) $ results

scanToFile :: (Show t1,Show t2) => String -> Bool -> [(CGen,t1,t2,Maybe Decomposition)] -> IO ()
scanToFile filename overwrite results = 
                    do
                        file <- openFile filename (if overwrite then WriteMode else AppendMode)
                        mapM_ (\result -> do 
                                hPutStrLn file $ printPlain result
                                hFlush file) results
                        hClose file

scanToFileAndStd :: (Show t1,Show t2) => String -> Bool -> [(CGen,t1,t2,Maybe Decomposition)] -> IO ()
scanToFileAndStd filename overwrite results = 
                    do
                        file <- openFile filename (if overwrite then WriteMode else AppendMode)
                        mapM_ (\result -> do 
                                let resultstr = printPlain result
                                putStrLn resultstr
                                hPutStrLn file resultstr
                                hFlush file) results
                        hClose file
--}}}
--Database Connection (ODBC) {{{
dsnName :: String
dsnName = "CIPConn"

--This function establishes a connection to the database and
--make sures the less than table is in the right format. If it is not, then the
--database has been somehow corrupted, and the connection function will throw
--a runtime exception. For definitions of the functions specific to the less-than
--database, see "Less-Than Database (HDBC)" below.
connectToDB :: IO Connection
connectToDB = do
                  conn <- connectODBC ("DSN=" ++ dsnName)
                  if not (ltTableExists conn)
                  then do ltMakeTable conn
                          return conn
                  else if not (ltTableValid conn)
                  then error ("Less-than Database has an invalidly formatted table!" ++
                              "Please salvage all important data, drop the table," ++
                              "and re-run this program.")
                  else return conn

makeLTDBConn :: IO (Connection,Statement,Statement)
makeLTDBConn = do
                  conn <- connectToDB
                  ltquery <- prepare conn ltQueryStr
                  ltstore <- prepare conn ltStoreStr
                  return (conn,ltquery,ltstore)

destroyLTDBConn :: (Connection,Statement,Statement) -> IO ()
destroyLTDBConn (conn,qst,sst) = do
                                    finish qst
                                    finish sst
                                    disconnect conn
                    
--}}}
--Less-Than Database (HDBC) {{{
ltExistStr :: String
ltExistStr = "SELECT COUNT(*) FROM information_schema.tables WHERE table_name='lessthans'"
ltTableExists :: Connection -> Bool
ltTableExists conn = (head . head) (unsafePerformIO $ quickQuery' conn ltExistStr []) == toSql (1 :: Int)

ltDescribeStr :: String
ltDescribeStr = "DESCRIBE lessthans"
ltTableValid :: Connection -> Bool
ltTableValid conn = let info = unsafePerformIO $ quickQuery' conn ltDescribeStr []
                    in length info == 4 && 
                       ((info !! 0) !! 0) == toSql "ctd1" && ((info !! 1) !! 0) == toSql "ctd2" &&
                       ((info !! 2) !! 0) == toSql "cgen" && ((info !! 3) !! 0) == toSql "lts" &&
                       ((info !! 0) !! 1) == toSql "varchar(40)" && ((info !! 1) !! 1) == toSql "varchar(40)" &&
                       ((info !! 2) !! 1) == toSql "varchar(40)" && ((info !! 3) !! 1) == toSql "longblob" &&
                       ((info !! 0) !! 2) == toSql "NO" && ((info !! 1) !! 2) == toSql "NO" &&
                       ((info !! 2) !! 2) == toSql "NO" && ((info !! 3) !! 2) == toSql "NO" &&
                       ((info !! 0) !! 3) == toSql "PRI" && ((info !! 1) !! 3) == toSql "PRI" &&
                       ((info !! 2) !! 3) == toSql "PRI" && 
                       ((info !! 0) !! 5) == toSql "" && ((info !! 1) !! 5) == toSql "" &&
                       ((info !! 2) !! 5) == toSql "" && ((info !! 3) !! 5) == toSql ""

ltMakeStr :: String
ltMakeStr = "CREATE TABLE lessthans (ctd1 VARCHAR(40)," ++
                                  "ctd2 VARCHAR(40)," ++
                                  "cgen VARCHAR(40)," ++
                                  "lts LONGBLOB NOT NULL," ++
                                  "PRIMARY KEY(ctd1,ctd2,cgen))"
ltMakeTable :: Connection -> IO Integer
ltMakeTable conn = run conn ltMakeStr []

ltQueryStr :: String
ltQueryStr = "SELECT lts FROM lessthans WHERE ctd1=? AND ctd2=? AND cgen=?"
{-# NOINLINE queryLTDB #-}
queryLTDB :: Statement -> CTD -> CTD -> CGen -> IO (Maybe [CGen])
queryLTDB queryst ctd ctd' cg' = do
                                        myLog 90 ("looking up lts in LTDB for ctd1 = " ++ show ctd ++ ", ctd2 = " ++ show ctd' ++ ", cgen = " ++ show cg')
                                        --Make a new connection each time
                                        conn <- connectODBC ("DSN=" ++ dsnName)
                                        ans <- quickQuery' conn ltQueryStr [toSql (show ctd),toSql (show ctd'),toSql (show cg')]
                                        disconnect conn
                                        --Use statement passed in
--                                        execute queryst [toSql (show ctd),
--                                                         toSql (show ctd'),
--                                                         toSql (show cg')]
--                                        ans <- fetchAllRows' queryst
                                        myLog 100 ("successfully looked up lts: " ++ show ctd ++ ", " ++ show ctd' ++ ", " ++ show cg' ++ ": " ++ abbrevListStr ans)
                                        if null ans then return Nothing
                                        else let res = fromRight
                                                   (decode (fromSql . head . head $ ans) :: Either String [CGen])
                                             in return $ Just res

ltStoreStr :: String
ltStoreStr = "INSERT INTO lessthans VALUES (?,?,?,?)"
{-# NOINLINE storeInLTDB #-}
storeInLTDB :: (Connection,Statement) -> CTD -> CTD -> CGen -> [CGen] -> IO Integer
storeInLTDB (conn,storest) ctd ctd' cg' lts = do
                                                      myLog 90 ("storing in LTDB: (" ++ show ctd ++ "," ++ show ctd' ++ "," ++ show cg' ++ "," ++ abbrevListStr lts ++ ")")
                                                      --Make a new connection each time
                                                      conn' <- connectODBC ("DSN=" ++ dsnName)
                                                      res <- run conn' ltStoreStr [toSql (show ctd),toSql (show ctd'),toSql (show cg'),toSql (encode lts)]
                                                      commit conn'
                                                      disconnect conn'
                                                      --Use connection and statement passed in
--                                                      res <- execute storest
--                                                                     [toSql (show ctd),toSql (show ctd'),
--                                                                      toSql (show cg'),toSql (encode lts)]
--                                                      commit conn
                                                      myLog 95 ("storing completed successfully")
                                                      return res
--}}}
