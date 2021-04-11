-- for finding index of element and table formatting
import Data.List (elemIndex, intercalate)
-- for pretty printing table output
import Text.Printf (printf)
-- for parsing user input into Maybe a
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
    north :: Int,
    east :: Int,
    populations :: [Int]
  }
  deriving (Eq, Ord, Show, Read)

type Location = (Int, Int)

-- accompanying function for above
location :: City -> Location
location c = (north c, east c)

--------------------------------------------------------
--                 multi-use functions                --
--------------------------------------------------------

-- means we can do (toFloat x) rather than (fromIntegral x :: Float)
-- thanks to the type inference
toFloat :: Integral a => a -> Float
toFloat = fromIntegral

-- divide by 1000 to convert from 1000s to millions
roundPopulation :: Integral a => a -> String
roundPopulation = printf "%.3fm" . (/1000) . toFloat

nameIndex :: String -> [City] -> Maybe Int
nameIndex = (. map name) . elemIndex

-- wrap type [a] either side with type [a]
wrap :: [a] -> [a] -> [a] -> [a]
wrap headr footr mid = headr ++ mid ++ footr

-- (Just a) if the index is valid, Nothing otherwise
maybeElemAt :: [a] -> Maybe Int -> Maybe a
maybeElemAt cs Nothing = Nothing
maybeElemAt cs (Just i)
  | i >= 0 && i < length cs = Just $ cs !! i
  | otherwise = Nothing

------------------------------------------------
--                  section one               --
------------------------------------------------

-- unlines changes e.x. ["London", "Paris"] to "London\nParis\n"
getCityNames :: [City] -> String
getCityNames = unlines . map name

-- for user interface, output a nicer formatted list
-- we could potentially do ... map ("* " ++) . lines . getCityNames
-- but it doesn't really improve readability much and means more list traversals
getPrettyNames :: [City] -> String
getPrettyNames = ("\nCities:\n\n" ++) . unlines . map (("* " ++) . name)

------------------------------------------------
--                  section two               --
------------------------------------------------

maybePopulationLength :: Maybe City -> Maybe Int
maybePopulationLength (Just city) = Just $ (length . populations) city
maybePopulationLength _ = Nothing

convertInputs :: String -> Int -> [City] -> (Maybe City, Maybe Int)
convertInputs cName yr cs = do
  let city = maybeElemAt cs (cName `nameIndex` cs)
  (city, maybePopulationLength city)

getYearData :: (Maybe City, Maybe Int) -> String
getYearData (Just c, Just i) = roundPopulation $ populations c !! i
getYearData _ = "no data"

getPopulation :: String -> Int -> [City] -> String
getPopulation n yr = getYearData . convertInputs n yr

--------------------------------------------------
--                  section three               --
--------------------------------------------------

getPopulationPair :: [Int] -> (String, String)
getPopulationPair (f : s : _) = (roundPopulation f, roundPopulation s)

getCityData :: City -> String
getCityData c = do
  let (popCur, popLast) = getPopulationPair $ populations c
  printf "%-12s   %12d   %12d   %12s   %12s"
    (name c) (north c) (east c) popCur popLast

citiesToString :: [City] -> String
citiesToString = unlines . map getCityData

-- the below are functions for making a pretty table,
-- used in the user interface section

getCityTableData :: City -> String
getCityTableData c = do
  let (popCur, popLast) = getPopulationPair $ populations c
  printf "\n| %-12s | %12d | %12d | %12s | %12s |"
    (name c) (north c) (east c) popCur popLast

citiesToTable :: [City] -> String
citiesToTable = wrap header columnLine . concatMap getCityTableData

header :: String
header =
  printf ( "%s\n|     Name     |  Deg. North  |   Deg. East  |"
      ++ "  Population  |  Last Years  |%s"
    ) columnLine columnLine

columnLine :: String
columnLine = wrap "\n+" "+" $ intercalate "+" (replicate 5 "--------------")

-------------------------------------------------
--                  section four               --
-------------------------------------------------

addYearToRecord :: City -> Int -> City
addYearToRecord c x = City (name c) (north c) (east c) (x : populations c)

updatePopulations :: [City] -> [Int] -> [City]
updatePopulations cs pops = do
  let (a, b) = splitAt (length pops) cs
  let a2 = zipWith addYearToRecord a pops
  a2 ++ b

-------------------------------------------------
--                  section five               --
-------------------------------------------------

insertSorted :: (Ord a) => a -> [a] -> [a]
insertSorted x xs@(next : rest)
  | x > next = next : insertSorted x rest
  | otherwise = x : xs
insertSorted x [] = [x]

------------------------------------------------
--                  section six               --
------------------------------------------------

calcFigures :: [Int] -> [Float]
calcFigures [c, p] = [toFloat (c - p) / toFloat p * 100]
calcFigures (c : p : ys) = calcFigures [c, p] ++ calcFigures (p : ys)

cityFromName :: [City] -> String -> Maybe City
cityFromName cs = maybeElemAt cs . (`nameIndex` cs)

getGrowth :: [City] -> String -> [Float]
getGrowth cs c = maybe [] (calcFigures . populations) $ cityFromName cs c

formatGrowthFigures :: String -> [City] -> String
formatGrowthFigures c cs = unlines [show g | g <- getGrowth cs c]

--------------------------------------------------
--                  section seven               --
--------------------------------------------------

distance :: Location -> Location -> Float
distance (x1, y1) (x2, y2) = sqrt . toFloat $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

getNameFromList :: [City] -> Int -> String
getNameFromList = (name .) . (!!)

findNearestCity :: Location -> Int -> [City] -> String
findNearestCity loc pop cs = do
  -- first filter out all the cities with a lower population
  let candidates = [c | c <- cs, head (populations c) > pop]
  -- then get [Float] array of their distances
  let distances = map (distance loc . location) candidates
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
demo 4 = putStr . citiesToString $ updatePopulations testData
  [1200, 3200, 3600, 2100, 1800, 9500, 6700, 11100, 4300, 1300, 2000, 1800]
demo 5 = putStr . citiesToString $ insertSorted 
  (City "Prague" 50 14 [1312, 1306, 1299, 1292]) testData
demo 6 = putStr $ formatGrowthFigures "London" testData
demo 7 = putStrLn $ findNearestCity (54, 6) 2000 testData
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

drawCityPlot :: Location -> String -> String -> IO ()
drawCityPlot (n, e) name pop = do
  -- multiply n + e by 2 to increase distance between points
  -- giving enough space to plot the population below city name
  let x = e * 2
  -- abs $ n - 54 (one larger than highest city) to invert y axis
  -- otherwise we're looking at the world upsidedown
  let y = round $ toFloat $ abs (n - 54) * 2
  writeAt (x, y) ("+ " ++ name)
  -- write population below city, otherwise populations + other cities overlap
  writeAt (x, y+1) pop

drawCity :: City -> IO ()
drawCity c = drawCityPlot (location c) (name c) (getYearData (Just c, Just 0))

drawCities :: [City] -> IO ()
drawCities c = do
  clearScreen
  mapM_ drawCity c -- mapM_ because map returns [IO ()] which we can't show
  -- move prompt to below mapped cities
  goTo (0, 40)

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
matchChoice choice cs
  | choice == "1" = putStr $ getPrettyNames cs
  | choice == "2" = doPopulationIO cs
  | choice == "3" = putStrLn $ citiesToTable cs
  | choice `elem` ["4", "5", "9"] = return () -- handle these options below
  | choice == "6" = doAnnualGrowthIO cs
  | choice == "7" = doFindCityIO cs
  | choice == "8" = drawCities cs
  | otherwise = putStrLn "Invalid choice"

readFileToCities :: FilePath -> IO [City]
readFileToCities = fmap (map read . lines) . readFile

loopChoices :: [City] -> IO String
loopChoices cs = do
  showOptions
  choice <- getLine
  matchChoice choice cs
  -- separate case to reduce complexity of above
  cs <- case choice of
    "4" -> do
      new <- doUpdatePopulationIO cs
      putStrLn $ citiesToTable new
      return new
    "5" -> do
      new <- doinsertSortedIO cs
      putStrLn $ citiesToTable new
      return new
    _ -> return cs

  -- convert cities from [City] to a newline separated String to write to file
  if choice == "9"
    then return (concatMap ((++ "\n") . show) cs)
    else loopChoices cs

main :: IO ()
main = do
  cs <- readFileToCities "cities.txt"
  putStr $ getPrettyNames cs
  cs <- loopChoices cs
  writeFile "cities.txt" cs

doFindCityIO :: [City] -> IO ()
doFindCityIO cs = do
  putStr "Please enter city's location (degrees north)\n> "
  north <- doGetInt
  putStr "Please enter city's location (degrees east):\n> "
  east <- doGetInt
  putStr "Please enter minimum population city should have:\n> "
  minPopulation <- doGetInt
  case (north, east, minPopulation) of
    (Just dN, Just dE, Just pop) ->
      putStrLn $ "\nNearest City: " ++ findNearestCity (dN, dE) pop cs
    _ -> putStrLn "Invalid population figure entered."

doCityNameIO :: IO String
doCityNameIO = do
  putStr "Please enter city name:\n> "
  getLine

doGetListOfInts :: IO [Int]
doGetListOfInts = do
  -- any non integer input terminates the recursion
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
  return (readMaybe input :: Maybe Int)

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
  putStrLn $
    "\nAnnual Growth Figures:\n"
      ++ unlines (printf "* %.2f%%" <$> getGrowth cs n)

-- <$> is the infix shortcut for fmap
doUpdatePopulationIO :: [City] -> IO [City]
doUpdatePopulationIO cs = updatePopulations cs <$> doGetListOfInts

doinsertSortedIO :: [City] -> IO [City]
doinsertSortedIO cs = do
  name <- doCityNameIO
  putStr "Please enter degrees north:\n> "
  north <- doGetInt
  putStr "Please enter degrees east:\n> "
  east <- doGetInt
  putStrLn "Please enter population figures (from most recent, 2+ entries)"
  populations <- doGetListOfInts
  case (north, east) of
    (Just n, Just e) -> return (insertSorted (City name n e populations) cs)
    _ -> do
      putStrLn "Invalid data given, not adding city"
      return cs
