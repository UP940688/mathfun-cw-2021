--
-- Types (define City type here)
--

testData :: [City]
testData = 
    [ City "Amsterdam" 52  5  [1158, 1149, 1140, 1132]
    , City "Athens"    38 23  [3153, 3153, 3154, 3156]
    , City "Berlin"    53 13  [3567, 3562, 3557, 3552]
    , City "Brussels"  51  4  [2096, 2081, 2065, 2050]
    , City "Bucharest" 44 26  [1794, 1803, 1812, 1821]
    , City "London"    52  0  [9426, 9304, 9177, 9046]
    , City "Madrid"    40  4  [6669, 6618, 6559, 6497]
    , City "Paris"     49  2  [11079, 11017, 10958, 10901]
    , City "Rome"      42 13  [4278, 4257, 4234, 4210]
    , City "Sofia"     43 23  [1284, 1281, 1277, 1272]
    , City "Vienna"    48 16  [1945, 1930, 1915, 1901]
    , City "Warsaw"    52 21  [1790, 1783, 1776, 1768] ]

data City = City {
    name :: String,
    degNorth :: Int,
    degEast :: Int,
    populationRecord :: [Int]
} deriving (Eq, Ord, Show, Read)


--
--  Your functional code goes here
--

-- demo one

print_names :: [City] -> IO ()
print_names = putStrLn . (++) "\nCity Names:\n\n" . format_names

format_names :: [City] -> String
format_names = foldr (++) "" . map (format_line . name)

format_line :: String -> String
format_line = ("* "++) . (++"\n")

-- demo two 

get_population_record :: City -> Int -> Int
get_population_record = (!!) . populationRecord

--  Demo
--

demo :: Int -> IO ()
demo 1 = print_names testData
demo 2 = print $ get_population_record (testData !! 6) 1
{--
demo 2 = -- output the population of "Madrid" 2 years ago
demo 3 = putStrLn (citiesToString testData)
demo 4 = -- output the data (as for (iii)) after it has been updated with the
         -- following new population figures (the first is for Amsterdam, etc.)
         -- [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800]
demo 5 = -- show the data (as for (iii)) after adding "Prague" (50N, 14E) 
         -- with population figures [1312, 1306, 1299, 1292]
demo 6 = -- output a list of annual growth figures for "London"
demo 7 = -- output the nearest city to location (54N ,6E) with 
         -- a population above 2m people
demo 8 = -- output the population map


--
-- Screen Utilities (use these to do the population map)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your population map code goes here
--



--
-- Your user interface (and loading/saving) code goes here
--
 
--}