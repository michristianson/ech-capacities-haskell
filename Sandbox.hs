import Obstruct

import System.IO
import System.IO.Unsafe
import Data.Time.Clock.POSIX
import Control.Applicative

paths = map (\i -> pathFromEdges [(i,-i)]) [1..20]
cgs = map makeAllEGen paths
ctd1 = Polydisk 2.43 1
ctd2 = Ellipsoid 3.205 3.205
ctd3 = Ellipsoid 100 100

b = 1.5
arange = (2.41,2.6,0.01)
crange = (-0.05,-0.01,0.01)
midc a = 2+a/2
makePolydisk a = Polydisk a 1
makeBall c = Ellipsoid (b*c) c
domain = ScanDomain makePolydisk makeBall cgs arange midc crange

outname = "Scan" ++ show b ++ ".txt"

--Runs a "squeeze scan" with the above parameters.
--main = scanToFileAndStd outname True $ squeezeScan domain thm114Heuristics

--Test generateLessThans for a range of different inputs.
polydisk = Polydisk 2.43 1
balls = [Ellipsoid 1 1, Ellipsoid 3 3, Ellipsoid 3.205 3.205, Ellipsoid 3.215 3.215,
         Ellipsoid 3.225 3.225, Ellipsoid 3.5 3.5, Ellipsoid 4 4, Ellipsoid 10 10]
testcgs = map (makeAllEGen . (\i -> pathFromEdges [(i,-i)])) [1..15]
printLTs :: Handle -> [CGen] -> IO ()
printLTs file [] = hPutStrLn file $ "No less thans found."
printLTs file lts = mapM_ ((hPutStrLn file) . (\(num,lt) -> show num ++ ": " ++ show lt)) $ zip [1..length lts] lts
main = do
          file <- openFile "Sandbox_Output.txt" WriteMode
          mapM_ (\(cg,ball) -> do 
                                  hPutStrLn file "----------------------------------------------------"
                                  hPutStrLn file $ "Less thans for: " ++ show cg
                                  hPutStrLn file $ "With respect to: " ++ show polydisk ++ " and " ++ show ball
                                  hPutStrLn file "----------------------------------------------------"
                                  putStrLn $ "Working on less thans for " ++ show cg
                                  putStrLn $ "with respect to " ++ show polydisk ++ " and " ++ show ball ++ "."
                                  printLTs file $ generateLessThan defaultHeuristics polydisk ball cg
                                  hFlush file)
                $ (,) <$> testcgs <*> balls
          hClose file
