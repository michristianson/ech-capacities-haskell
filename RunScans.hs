import Obstruct

ds = [20..21]
-- This contains (start value, end value, step value) for the range of a-values
-- to use. As currently written, it includes the values
-- 2.41, 2.42, 2.43, . . . , 2.68, 2.69, 2.70.
arange = (2.41,2.70,0.01)
-- This contains (start value, end value, step value), just like arange above,
-- but this time for OFFSETS from the output of midc.
-- As currently written, for every a-value, we will use the c-values
-- (midc a) - 0.05, (midc a) - 0.04, (midc a) - 0.03, (midc a) - 0.02, (midc a) - 0.01, (midc a).
crange = (-0.05,0,0.01)
-- Function to compute a "default" c-value to use for each a-value.
midc a = 2+a/2

-- The list of CGen's to use for the scan.
cgs = map (\i -> makeAllEGen $ pathFromEdges [(i,-i)]) ds
-- The function to make our ctds. The range of a-values used will be determined
-- by arange, which is defined above.
makePolydisk a = Polydisk a 1
-- The function to make our ctd's. The range of c-values used will be determined
-- by crange, which is defined above.
makeBall c = Ellipsoid c c

domain = ScanDomain makePolydisk makeBall cgs arange midc crange
heuristics = thm114Heuristics

outname = if length ds == 1 then "Scan" ++ (show (head ds)) ++ ".txt"
                            else "Scan" ++ (show (head ds)) ++ "-" ++ (show (last ds)) ++ ".txt"

main = scanToFileAndStd outname True $ squeezeScan domain heuristics
