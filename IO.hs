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

logLevel :: Int
logLevel = 80

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
