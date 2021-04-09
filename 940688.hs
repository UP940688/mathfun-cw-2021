-- for finding index of element and table formatting
import Data.List (elemIndex, intercalate)
-- for pretty printing table output
import Text.Printf (printf)
-- for parsing user input into Maybe (Type)
import Text.Read (readMaybe)

------------------------------------------------
--                   types                    --
------------------------------------------------

testData :: [City]
testData =
  [ City "Amsterdam" 52 5 [1158, 1149, 1140, 1132],
    City "Athens" 38 23 [3153, 3153, 3154, 3156],
    City "Berlin" 53 13 [3567, 3562, 3557, 3552],
    City "Brussels" 51 4 [2096, 2081, 2065, 2050],
    City "Bucharest" 44 26 [1794, 1803, 1812, 1821],
    City "London" 52 0 [9426, 9304, 9177, 9046],
    City "Madrid" 40 4 [6669, 6618, 6559, 6497],
    City "Paris" 49 2 [11079, 11017, 10958, 10901],
    City "Rome" 42 13 [4278, 4257, 4234, 4210],
    City "Sofia" 43 23 [1284, 1281, 1277, 1272],
    City "Vienna" 48 16 [1945, 1930, 1915, 1901],
    City "Warsaw" 52 21 [1790, 1783, 1776, 1768]
  ]

data City = City
  { name :: String,
    degNorth :: Int,
    degEast :: Int,
    populationRecord :: [Int]
  }
  deriving (Show, Read)

type Location = (Int, Int)

--------------------------------------------------------
--                  helper functions                  --
--------------------------------------------------------

-- means we can do (toFloat x) rather than (fromIntegral x :: Float)
toFloat :: Int -> Float
toFloat = fromIntegral

nameIndex :: String -> [City] -> Maybe Int
nameIndex n = elemIndex n . map name

------------------------------------------------
--                  section one               --
------------------------------------------------

-- unlines changes e.x. ["a", "b"] to "a\nb\n"
getCityNames :: [City] -> String
getCityNames = unlines . map name

-- for main, output a nicer formatted list
getPrettyNames :: [City] -> String
getPrettyNames = ("\nCities:\n\n" ++) . unlines . map (("* " ++) . name)

------------------------------------------------
--                  section two               --
------------------------------------------------

-- return Nothing if year is above max
validYear :: Int -> Int -> Maybe Int
validYear yr max
  | yr >= 0 && yr < max = Just yr
  | otherwise = Nothing

-- idx must be validated as a safe number before calling this function
cityMaybe :: Maybe Int -> Maybe City
cityMaybe (Just i) = Just $ testData !! i
cityMaybe _ = Nothing

-- take a user or program supplied string + int, convert them
-- to Just / Nothing values depending on checks passed
convertInputs :: String -> Int -> [City] -> (Maybe City, Maybe Int)
convertInputs n yr cs = do
  let city = cityMaybe $ n `nameIndex` cs
  (city, validYear yr =<< lenPopMaybe city)

lenPopMaybe :: Maybe City -> Maybe Int
lenPopMaybe (Just city) = Just $ (length . populationRecord) city
lenPopMaybe _ = Nothing

getPopulationString :: (Maybe City, Maybe Int) -> String
getPopulationString (Just c, Just i) = (formatPopulation . toFloat) $ populationRecord c !! i
getPopulationString _ = "no data"

-- format population from  X thousands to X millions (to 3 d.p.)
formatPopulation :: Float -> String
formatPopulation = printf "%.3fm" . flip (/) 1000

getPopulation :: String -> Int -> [City] -> String
getPopulation yr cs = getPopulationString . convertInputs yr cs

--------------------------------------------------
--                  section three               --
--------------------------------------------------

getCityData :: City -> String
getCityData c = do
  let popCur = formatPopulation . toFloat $ head (populationRecord c)
  let popLast = formatPopulation . toFloat $ populationRecord c !! 1
  printf "%-12s   %12d   %12d   %12s   %12s" (name c) (degNorth c) (degEast c) popCur popLast

citiesToString :: [City] -> String
citiesToString = unlines . map getCityData

-- the below are functions for making a pretty table,
-- used in the user interface section

getCityTableData :: City -> String
getCityTableData c = do
  let popCur = formatPopulation . toFloat $ head (populationRecord c)
  let popLast = formatPopulation . toFloat $ populationRecord c !! 1
  printf "\n| %-12s | %12d | %12d | %12s | %12s |" (name c) (degNorth c) (degEast c) popCur popLast

citiesToTable :: [City] -> String
citiesToTable = (++) header . foldr ((++) . getCityTableData) columnLine

header :: String
header =
  printf
    "%s\n|     Name     |  Deg. North  |   Deg. East  |  Population  |  Last Years  |%s"
    columnLine
    columnLine

columnLine :: String
columnLine = "\n+" ++ intercalate "+" (replicate 5 "--------------") ++ "+"

-------------------------------------------------
--                  section four               --
-------------------------------------------------

addYearToRecord :: (City, Int) -> City
addYearToRecord (c, x) = City (name c) (degNorth c) (degEast c) (x : populationRecord c)

updatePopulations :: [City] -> [Int] -> [City]
updatePopulations = zipWith (curry addYearToRecord)

formatNewPopulations :: [Int] -> [City] -> String
formatNewPopulations = (citiesToString .) . flip updatePopulations

-------------------------------------------------
--                  section five               --
-------------------------------------------------

insertNewCity :: City -> [City] -> [City]
insertNewCity c [] = [c]
insertNewCity c (cn : cs)
  | name c > name cn = cn : insertNewCity c cs
  | otherwise = c : cn : cs

formatNewCities :: City -> [City] -> String
formatNewCities = (citiesToString .) . insertNewCity

------------------------------------------------
--                  section six               --
------------------------------------------------

calcFigures :: [Int] -> [Float]
calcFigures [c, p] = [toFloat (c - p) / toFloat p * 100]
calcFigures (c : p : ys) = calcFigures [c, p] ++ calcFigures (p : head ys : tail ys)

getCityGrowthFigures :: City -> [Float]
getCityGrowthFigures = calcFigures . populationRecord

getFigsFromName :: String -> [City] -> [Float]
getFigsFromName n cs = (maybe [] getCityGrowthFigures . cityMaybe) $ nameIndex n cs

getGrowthFigures :: String -> [City] -> String
getGrowthFigures c cs = unlines $ map show $ getFigsFromName c cs

--------------------------------------------------
--                  section seven               --
--------------------------------------------------

distanceBetween :: Location -> Location -> Float
distanceBetween (x1, y1) (x2, y2) = sqrt . toFloat $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

higherPopulation :: Int -> City -> Bool
higherPopulation = flip $ (>) . head . populationRecord

zipLocations :: [City] -> [Location]
zipLocations c = zip (map degNorth c) (map degEast c)

getNameFromList :: [City] -> Int -> String
getNameFromList = (name .) . (!!)

findNearestCity :: Location -> Int -> [City] -> String
findNearestCity loc population cities = do
  -- first filter out all the cities with a lower population
  let candidates = filter (higherPopulation population) cities
  -- then get [Float] array of their distances
  let distances = map (distanceBetween loc) $ zipLocations candidates
  -- get index of city with smallest distance (Maybe Int)
  let idx = elemIndex (minimum distances) distances
  maybe "no city" (getNameFromList candidates) idx

------------------------------------------------------
--                  demo execution                  --
------------------------------------------------------

demo :: Int -> IO ()
demo 1 = putStr $ getCityNames testData
demo 2 = putStrLn $ getPopulation "Madrid" 2 testData
demo 3 = putStr $ citiesToString testData
demo 4 = putStr $ formatNewPopulations [1200, 3200, 3600, 2100, 1800, 9500, 6700, 11100, 4300, 1300, 2000, 1800] testData
demo 5 = putStr $ formatNewCities (City "Prague" 50 14 [1312, 1306, 1299, 1292]) testData
demo 6 = putStr $ getGrowthFigures "London" testData
demo 7 = putStrLn $ findNearestCity (54, 6) 2000 testData
-- WIP
demo 8 = drawCities testData

----------------------------------------------------
--                  screen utils                  --
----------------------------------------------------

type ScreenPosition = (Int, Int)

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

------------------------------------------------------
--                  population map                  --
------------------------------------------------------

writeBox :: Location -> String -> String -> IO ()
writeBox (n, e) name pop = do
  writeAt (n, e) ("+ " ++ name)
  writeAt (n, e + 2) pop

drawCity :: City -> IO ()
drawCity city = do
  let location = (degNorth city, degEast city)
  let cityName = name city
  let population = getPopulationString (Just city, Just 0)
  writeBox location cityName population

-- we need mapM because just {f}map returns [IO ()] which we can't print
-- mapM_ because we don't care about outputting the end result [(), (), ...]
drawCities :: [City] -> IO ()
drawCities c = do
  clearScreen
  mapM_ drawCity c

------------------------------------------------------
--                  user interface                  --
------------------------------------------------------

showOptions :: IO ()
showOptions =
  putStr $
    "\nOptions:\n(1) Show names"
      ++ "\n(2) Return population of city n years ago"
      ++ "\n(3) Display cities data"
      ++ "\n(4) Update data to include new population figures"
      ++ "\n(5) Add a new city to the data"
      ++ "\n(6) Get annual population growth percentage figs for a city"
      ++ "\n(7) Locate nearest city to given point above given population"
      ++ "\n(8) Draw city map"
      ++ "\n(9) Exit\n> "

matchChoice :: String -> [City] -> IO ()
matchChoice choice cities
  | choice == "1" = putStr $ getPrettyNames cities
  | choice == "2" = doPopulationIO cities
  | choice == "3" = putStrLn $ citiesToTable cities
  | choice `elem` ["4", "5", "9"] = return () -- handle these options seperately
  | choice == "6" = doAnnualGrowthIO cities
  | choice == "7" = doFindCityIO cities
  | choice == "8" = drawCities cities
  | otherwise = putStrLn "Invalid choice"

readFileToCities :: FilePath -> IO [City]
readFileToCities = fmap (map read . lines) . readFile

loopChoices :: [City] -> IO String
loopChoices cities = do
  showOptions
  choice <- getLine
  matchChoice choice cities
  -- separate case to reduce complexity of above
  cities <- case choice of
    "4" -> do
      new <- doUpdatePopulationIO cities
      putStrLn $ citiesToTable new
      return new
    "5" -> do
      new <- doInsertNewCityIO cities
      putStrLn $ citiesToTable new
      return new
    _ -> return cities

  -- convert cities from [City] to a newline separated String to write to the file
  if choice /= "9" then loopChoices cities else return (concatMap ((++ "\n") . show) cities)

main :: IO ()
main = do
  cities <- readFileToCities "cities.txt"
  putStr $ getPrettyNames cities
  cities <- loopChoices cities
  writeFile "cities.txt" cities

doFindCityIO :: [City] -> IO ()
doFindCityIO cities = do
  putStrLn "Please enter location in degrees seperately (north first, then east):"
  location <- doGetListOfInts
  putStr "Please enter minimum population city should have:\n> "
  minPopulation <- doGetInt
  if length location /= 2
    then do
      putStrLn "Invalid locations entered"
      return ()
    else case minPopulation of
      (Just pop) -> putStrLn $ "\nNearest City: " ++ findNearestCity (head location, last location) pop cities
      _ -> putStrLn "Invalid population figure entered."

doCityNameIO :: IO String
doCityNameIO = do
  putStr "Please enter city name:\n> "
  getLine

doGetListOfInts :: IO [Int]
doGetListOfInts = do
  putStr "Please enter an integer ('exit' to finish)\n> "
  input <- doGetInt
  case input of
    (Just int) -> do
      nextInput <- doGetListOfInts
      return (int : nextInput)
    Nothing -> return []

doGetInt :: IO (Maybe Int)
doGetInt = do
  input <- getLine
  let converted = readMaybe input :: Maybe Int
  return converted

doPopulationIO :: [City] -> IO ()
doPopulationIO cities = do
  city <- doCityNameIO
  putStr "Please enter how many years ago to get records (0 for current)\n> "
  idx <- doGetInt
  case idx of
    (Just i) -> putStrLn $ "\nPopulation: " ++ getPopulation city i cities
    Nothing -> putStrLn "Invalid index"

doAnnualGrowthIO :: [City] -> IO ()
doAnnualGrowthIO cs = do
  n <- doCityNameIO
  putStrLn $ "\nAnnual Growth Figures:\n" ++ unlines (printf "* %.2f%%" <$> getFigsFromName n cs)

-- <$> is the infix shortcut for fmap
doUpdatePopulationIO :: [City] -> IO [City]
doUpdatePopulationIO cities = updatePopulations cities <$> doGetListOfInts

doInsertNewCityIO :: [City] -> IO [City]
doInsertNewCityIO cities = do
  name <- doCityNameIO
  putStr "Please enter degrees north:\n> "
  degNorth <- doGetInt
  putStr "Please enter degrees east:\n> "
  degEast <- doGetInt
  putStrLn "Please enter population figures (from most recent, at least two entries)"
  populations <- doGetListOfInts
  case (degNorth, degEast) of
    (Just dN, Just dE) -> return (cities ++ [City name dN dE populations])
    _ -> do
      putStrLn "Invalid data given, not adding city"
      return cities