import Data.List
import Data.Maybe
import Text.Printf

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
} deriving (Show, Read)

--
--  Your functional code goes here
--

-- demo one

printNames :: [City] -> IO ()
printNames = putStrLn . (++) "\nCity Names:\n\n" . formatNames

formatNames :: [City] -> String
formatNames = foldr (++) "" . formatLines . getNames

formatLines :: [String] -> [String]
formatLines = map $ ("* "++) . (++"\n")

getNames :: [City] -> [String]
getNames = map name

-- demo two

getNameIdx :: String -> Maybe Int
getNameIdx =  flip elemIndex $ getNames testData

validYear :: Int -> Int -> Maybe Int
validYear yr yrLen
  | (yr >= 0) && (yr < yrLen) = Just yr
  | otherwise = Nothing

cityMaybe :: Maybe Int -> Maybe City
cityMaybe (Just idx) = Just (testData !! idx)
cityMaybe _ = Nothing

lenPopMaybe :: Maybe City -> Maybe Int
lenPopMaybe (Just city) = Just (length $ populationRecord city)
lenPopMaybe _ = Nothing

convertInputs :: String -> Int -> (Maybe City, Maybe Int)
convertInputs n yr = do
    let city = cityMaybe $ getNameIdx n
    let year = maybe Nothing (validYear yr) (lenPopMaybe city)
    (city, year)

getPopulationString :: (Maybe City, Maybe Int) -> String
getPopulationString (Just city, Just popIdx) = do
    let population = (populationRecord city) !! popIdx
    formatPopulation (fromIntegral population :: Float)
getPopulationString _ = "no data"  

getPopulation :: String -> Int -> String
getPopulation n yr = do
    let (city, year) = convertInputs n yr
    getPopulationString (city, year)

printPopulation :: String -> Int -> IO ()
printPopulation n yr = putStrLn $ "\nPopulation: " ++ (getPopulation n yr) ++ "\n"

formatPopulation :: Float -> String
formatPopulation = printf "%.3fm" . flip (/) 1000

-- demo three

getCityData c = do
    let locNorth = (show $ degNorth c)
    let locEast = (show $ degEast c)
    let cur = (!!) (populationRecord c) 0
    let popCur = formatPopulation (fromIntegral cur ::  Float)
    let last = (!!) (populationRecord c) 1
    let popLast = formatPopulation (fromIntegral last ::  Float)
    printf "\n| %-12s | %14s | %14s | %12s | %13s |" (name c) locNorth locEast popCur popLast

rowLine = "\n---------------------------------------------------------------------------------"

header :: String
header = do
    let l1 = printf "\n|     Name     |  Degrees North |  Degrees East  |  Population  |  Last Year's  |"
    rowLine ++ l1 ++ rowLine

citiesToString :: [City] -> String
citiesToString = (++) header . foldr (++) (rowLine++"\n") . map getCityData

-- demo four

insertElem :: Int -> [Int] -> [Int]
insertElem = (:)

updatePopulation :: (City, Int) -> City
updatePopulation (c, n) = do
    let oldFigures = populationRecord c
    let populationRecord c = n:oldFigures
    City (name c) (degNorth c) (degEast c) (populationRecord c)

updatePopulations :: [(City, Int)] -> [City]
updatePopulations = map (updatePopulation)

newPopulations cs np = updatePopulations $ zip cs np

doUpdatePopulations np = do
    let newPops = newPopulations testData np
    let testData = newPops
    putStrLn (citiesToString testData)

--  Demo
--

demo :: Int -> IO ()
demo 1 = printNames testData
demo 2 = printPopulation "Madrid" 2
demo 3 = putStrLn (citiesToString testData)
demo 4 = doUpdatePopulations [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800]
{--
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

readCities :: [String] -> [City]
readCities = map read

readFileToLines = fmap (lines) . readFile
readFileToCities = fmap (readCities) . readFileToLines

getCityNames = do
    cities <- readFileToCities "cities.txt"
    let names = map (name) cities
    putStrLn $ foldr (++) "" names
 
--}
