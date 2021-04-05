import Data.List ( elemIndex, intercalate )
import Data.Maybe ()
import Text.Printf ( printf, PrintfType )

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

printNames :: [City] -> IO ()
printNames = putStrLn . (++) "\nCity Names:\n\n" . formatNames

formatNames :: [City] -> String
formatNames = concat . formatLines . getNames

formatLines :: [String] -> [String]
formatLines = map $ ("* "++) . (++"\n")

getNames :: [City] -> [String]
getNames = map name

-- demo two

toFloat :: Int -> Float
toFloat = fromIntegral

getNameIdx :: String -> Maybe Int
getNameIdx =  flip elemIndex $ getNames testData

validYear :: Int -> Int -> Maybe Int
validYear yr yrLen
  | yr >= 0 && yr < yrLen = Just yr
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
    let year = validYear yr =<< lenPopMaybe city
    (city, year)

getPopulationString :: (Maybe City, Maybe Int) -> String
getPopulationString (Just city, Just popIdx) = do
    let population = populationRecord city !! popIdx
    formatPopulation $ toFloat population
getPopulationString _ = "no data"

getPopulation :: String -> Int -> String
getPopulation n yr = do
    let (city, year) = convertInputs n yr
    getPopulationString (city, year)

printPopulation :: String -> Int -> IO ()
printPopulation c yr = putStrLn $ "\nPopulation: " ++ getPopulation c yr ++ "\n"

formatPopulation :: Float -> String
formatPopulation = printf "%.3fm" . flip (/) 1000

-- demo three

getCityData :: PrintfType t => City -> t
getCityData c = do
    let popCur = formatPopulation . toFloat $ head (populationRecord c)
    let popLast = formatPopulation . toFloat $ populationRecord c !! 1
    printf "\n| %-14s | %14d | %14d | %14s | %14s |" (name c) (degNorth c) (degEast c) popCur popLast

columnLine :: String
columnLine = "\n+" ++ intercalate "+" (replicate 5 "----------------") ++ "+"

header :: String
header = printf
    "%s\n|      Name      |  Degrees North |  Degrees East  |   Population   |   Last Years   |%s"
    columnLine columnLine

citiesToString :: [City] -> String
citiesToString = (++) header . foldr ((++) . getCityData) (columnLine++"\n")

-- demo four

addYearToRecord :: (City, Int) -> City
addYearToRecord (c, n) = do
    let oldFigures = populationRecord c
    let populationRecord c = n:oldFigures
    City (name c) (degNorth c) (degEast c) (populationRecord c)

updatePopulations :: [City] -> [Int] -> [City]
updatePopulations = zipWith (curry addYearToRecord)

printNewPopulations :: [Int] -> IO ()
printNewPopulations = putStrLn . citiesToString . updatePopulations testData

-- demo five

insertNewCity :: [City] -> City -> [City]
insertNewCity [] c = [c]
insertNewCity (nc:cs) c
  | name c > name nc     = nc : insertNewCity cs c
  | otherwise            = c:nc:cs

printNewCities :: City -> IO ()
printNewCities = putStrLn . citiesToString . insertNewCity testData

-- demo six

growthFigures :: [Int] -> [Float]
growthFigures [cy, py] = [toFloat (cy-py) / toFloat py * 100]
growthFigures (cy:py:ys) = growthFigures [cy, py] ++ growthFigures (py:head ys:tail ys)

formatGrowthFigures :: [Float] -> String
formatGrowthFigures = concatMap $ printf "\n* %.2f%%"

getCityGrowthFigures :: City -> String
getCityGrowthFigures c = formatGrowthFigures . growthFigures $ populationRecord c

printAnnualGrowth :: String -> IO ()
printAnnualGrowth cname = do
    let city = cityMaybe $ getNameIdx cname
    putStrLn $ printf "\nPopulation Growth (%s):\n" cname ++ maybe "\nno data" getCityGrowthFigures city ++ "\n"

--  Demo
--

demo :: Int -> IO ()
demo 1 = printNames testData
demo 2 = printPopulation "Madrid" 2
demo 3 = putStrLn (citiesToString testData)
demo 4 = printNewPopulations [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800]
demo 5 = printNewCities (City "Prague" 50 14 [1312, 1306, 1299, 1292])
demo 6 = printAnnualGrowth "London"
{--
demo 7 = -- output the nearest city to location (54N ,6E) with 
         -- a population above 2m people
demo 8 = -- output the population map
--}

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

readFileToLines :: FilePath -> IO [String]
readFileToLines = fmap lines . readFile

readFileToCities :: FilePath -> IO [City]
readFileToCities = fmap readCities . readFileToLines

getCityNames :: IO ()
getCityNames = do
    cities <- readFileToCities "cities.txt"
    let names = map name cities
    putStrLn $ concat names
