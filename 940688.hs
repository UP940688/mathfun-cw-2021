-- for finding index of element and table formatting
import Data.List (elemIndex, intercalate)
-- for filtering Nothings out of [Maybe a]
import Data.Maybe (catMaybes)
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

-- below types defined for easier reading of type signatures

type Location = (Int, Int)

type Population = Int

type Index = Int

type Distance = Float

type Growth = Float

type StringPopulation = String

type Name = String

--------------------------------------------------------
--                  helper functions                  --
--------------------------------------------------------

toFloat :: Integral a => a -> Float
toFloat = fromIntegral

location :: City -> Location
location c = (north c, east c)

locations :: [City] -> [Location]
locations = map location

nameIndex :: [City] -> Name -> Maybe Index
nameIndex = flip elemIndex . map name

-- (Just a) if the index is valid, Nothing otherwise
maybeElemAt :: [a] -> Maybe Index -> Maybe a
_ `maybeElemAt` Nothing = Nothing
cs `maybeElemAt` (Just i)
  | i >= 0 && i < length cs = Just $ cs !! i
  | otherwise = Nothing

cityFromName :: [City] -> Name -> Maybe City
cityFromName cs = maybeElemAt cs . nameIndex cs

------------------------------------------------
--                  section one               --
------------------------------------------------

getNames :: [City] -> [Name]
getNames = map name

------------------------------------------------
--                  section two               --
------------------------------------------------

getPopulation :: [City] -> Name -> Index -> StringPopulation
getPopulation = (populationIndex .) . cityFromName

populationIndex :: Maybe City -> Index -> StringPopulation
Nothing `populationIndex` _ = "no data"
(Just c) `populationIndex` i
  | i >= 0 && i < len = fmtPopulation (populations c !! i)
  | otherwise = "no data"
  where
    len = (length . populations) c

-- divide by 1000 to convert from x 1000s to x millions
fmtPopulation :: Population -> StringPopulation
fmtPopulation = printf "%.3fm" . (/ 1000) . toFloat

--------------------------------------------------
--                  section three               --
--------------------------------------------------

citiesToString :: [City] -> String
citiesToString = wrap header columnLine . concatMap getCityData

getCityData :: City -> String
getCityData c = printf "| %-12s | %12d | %12d | %12s | %12s |\n"
  (name c) (north c) (east c) cur pvs
  where
    (cur, pvs) = getPopulationPair $ populations c

getPopulationPair :: [Population] -> (StringPopulation, StringPopulation)
getPopulationPair (cur : pvs : _) = (fmtPopulation cur, fmtPopulation pvs)
getPopulationPair [cur] = (fmtPopulation cur, "no data")
getPopulationPair _ = ("no data", "no data")

wrap :: [a] -> [a] -> [a] -> [a]
wrap headr footr mid = headr ++ mid ++ footr

header :: String
header = wrap columnLine ('\n' : columnLine)
  "|     Name     |  Deg. North  |   Deg. East  |  Population  |   Previous   |"

columnLine :: String
columnLine = wrap "+" "+\n" $ intercalate "+" (replicate 5 "--------------")

-------------------------------------------------
--                  section four               --
-------------------------------------------------

updatePopulations :: [City] -> [Population] -> [City]
updatePopulations cs pops = changed ++ unchanged
  where
    changed = zipWith addYearToRecord cs pops
    unchanged = drop (length changed) cs

addYearToRecord :: City -> Population -> City
addYearToRecord c p = City (name c) (north c) (east c) (p : populations c)

-------------------------------------------------
--                  section five               --
-------------------------------------------------

sortedInsert :: (Ord a) => [a] -> a -> [a]
[] `sortedInsert` x = [x]
xs@(next : rest) `sortedInsert` x
  | x > next = next : rest `sortedInsert` x
  | otherwise = x : xs

------------------------------------------------
--                  section six               --
------------------------------------------------

fmtGrowthFigures :: [City] -> Name -> String
fmtGrowthFigures cs nm = unlines [show g | g <- getGrowth cs nm]

getGrowth :: [City] -> Name -> [Growth]
getGrowth cs nm = maybe [] growthList city
  where
    growthList = calcGrowth . populations
    city = cityFromName cs nm

calcGrowth :: [Population] -> [Growth]
calcGrowth [cur, pvs] = [toFloat (cur - pvs) / toFloat pvs * 100]
calcGrowth (cur : pvs : ys) = calcGrowth [cur, pvs] ++ calcGrowth (pvs : ys)
calcGrowth _ = [0]

--------------------------------------------------
--                  section seven               --
--------------------------------------------------

nearestCityName :: [City] -> Location -> Population -> Name
nearestCityName cs loc pop = maybe "no data" name (nearestCity cs loc pop)

nearestCity :: [City] -> Location -> Population -> Maybe City
nearestCity cs target pop = idx >>= \i -> Just (cs !! i)
  where
    candidates = [c | c <- cs, head (populations c) > pop]
    dists = target `distancesFrom` locations candidates
    idx = elemIndex (minimum dists) dists

distancesFrom :: Location -> [Location] -> [Distance]
distancesFrom = map . distance

distance :: Location -> Location -> Distance
distance (x1, y1) (x2, y2) = sqrt . toFloat $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

------------------------------------------------------
--                  demo execution                  --
------------------------------------------------------

demo :: Int -> IO ()
demo 1 = print $ getNames testData
demo 2 = putStrLn $ getPopulation testData "Madrid" 2
demo 3 = putStr $ citiesToString testData
demo 4 = putStr . citiesToString $ updatePopulations testData
  [1200, 3200, 3600, 2100, 1800, 9500, 6700, 11100, 4300, 1300, 2000, 1800]
demo 5 = putStr . citiesToString $ 
  testData `sortedInsert` City "Prague" 50 14 [1312, 1306, 1299, 1292]
demo 6 = putStr $ fmtGrowthFigures testData "London"
demo 7 = putStrLn $ nearestCityName testData (54, 6) 2000
demo 8 = drawCities testData
demo _ = putStrLn "Please pick a number 1-8."

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

-- TODO: ask if we can use ansi colours in escape

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
  goTo position
  putStr text

------------------------------------------------------
--                  population map                  --
------------------------------------------------------

-- IO is a monad, so use mapM to apply drawCity to each city
-- pass the mapped co-ords to adjustCursor
drawCities :: [City] -> IO ()
drawCities cs = clearScreen >> mapM drawCity cs >>= adjustCursor

-- move the cursor to four lines below the lowest mapped city
adjustCursor :: [Int] -> IO ()
adjustCursor positions = goTo (0, maximum positions + 4)

drawCity :: City -> IO Int
drawCity c = drawCityPlot (location c) (name c) (Just c `populationIndex` 0)

drawCityPlot :: Location -> Name -> StringPopulation -> IO Int
drawCityPlot loc nm pop = do
  let (x, y) = locationToPosition loc
  writeAt (x, y) ("+ " ++ nm)
  writeAt (x, y + 1) pop
  return y

-- get absolute value after subtracting 54 (largest nm value in testData) to
-- flip y axis, multiply by 2 to proportionately increase distance between cities
locationToPosition :: Location -> ScreenPosition
locationToPosition (n, e) = (e * 2, abs (n - 54) * 2)

------------------------------------------------------
--                  user interface                  --
------------------------------------------------------

main :: IO ()
main = do
  tmp <- fileToCities "cities.txt"
  -- we filter out any invalid cities given to us in the file
  -- and notify the user how many have been filtered out
  let cs = catMaybes tmp
      diff = length tmp - length cs
  if diff /= 0
    then printf "\nWARNING: Ignored %i rows of invalid data in file.\n" diff
    else return ()
  putStrLn $ '\n' : getPrettyNamesString cs
  updatedCities <- loopChoices cs
  writeFile "cities.txt" updatedCities

fileToCities :: FilePath -> IO [Maybe City]
fileToCities = fmap (map readMaybe . lines) . readFile

showOptions :: IO ()
showOptions = putStr $ underline "Options:" ++ "\n\n\
  \(1) Show names \n\
  \(2) Return population of city n years ago \n\
  \(3) Display cities data \n\
  \(4) Update data to include new population figures \n\
  \(5) Add a new city to the data \n\
  \(6) Get annual population growth percentage figs for a city \n\
  \(7) Locate nearest city to given point above given population \n\
  \(8) Draw city map \n\
  \(9) Exit"

 -- TODO: need to ask if this is okay
promptUser :: IO String
promptUser = do
  putStr "\n\n\ESC[2m>>> "
  input <- getLine
  putStr "\ESC[0m\n"
  return input

underline :: String -> String
underline s = "\ESC[1;4m" ++ s ++ "\ESC[0m"

red s = "\ESC[31m" ++ s ++ "\ESC[0m"

matchChoice :: [City] -> String -> IO ()
matchChoice cs choice
  | choice == "1" = putStrLn $ getPrettyNamesString cs
  | choice == "2" = doPopulationIO cs
  | choice == "3" = putStrLn $ citiesToString cs
  | choice == "6" = doAnnualGrowthIO cs
  | choice == "7" = doFindCityIO cs
  | choice == "8" = drawCities cs
  | choice `elem` ["4", "5", "9"] = return () -- handle these options below
  | otherwise = putStrLn $ red "Invalid choice.\n"

loopChoices :: [City] -> IO String
loopChoices cs = do
  showOptions
  choice <- promptUser
  matchChoice cs choice
  -- separate case to reduce complexity of above
  updatedCities <- case choice of
    "4" -> do
      new <- fmap (updatePopulations cs) getListOfIntsIO
      putStrLn $ citiesToString new
      return new
    "5" -> do
      new <- dosortedInsertIO cs
      putStrLn $ citiesToString new
      return new
    _ -> return cs

  if choice == "9"
    then -- convert cities from [City] to newline-separated String
      return (unlines $ map show updatedCities)
    else loopChoices updatedCities

-- for user interface, output a nicer formatted list
-- unlines changes e.x. ["London", "Paris"] to "London\nParis\n"
getPrettyNamesString :: [City] -> String
getPrettyNamesString = (underline "Cities:\n\n" ++) .
  unlines . map (("• " ++) . name)

getCityNameIO :: IO Name
getCityNameIO = putStr "Please enter city name:" >> promptUser

getIntIO :: IO (Maybe Int)
getIntIO = readMaybe <$> promptUser

getListOfIntsIO :: IO [Int]
getListOfIntsIO = do
  -- any non integer input terminates the recursion
  putStr "Please enter an integer (non-integer to finish)"
  input <- getIntIO
  case input of
    -- lambda needed due to the IO monad
    (Just x) -> getListOfIntsIO >>= \xs -> return (x : xs)
    Nothing -> return []

doFindCityIO :: [City] -> IO ()
doFindCityIO cs = do
  putStr "Please enter city's location (degrees north):"
  nrth <- getIntIO
  putStr "Please enter city's location (degrees east):"
  est <- getIntIO
  putStr "Please enter minimum population city should have:"
  minPopulation <- getIntIO
  case (nrth, est, minPopulation) of
    (Just n, Just e, Just pop) ->
      printf "Nearest City: %s\n\n" (nearestCityName cs (n, e) pop)
    _ -> putStrLn $ red "Invalid population figure entered."

doPopulationIO :: [City] -> IO ()
doPopulationIO cs = do
  city <- getCityNameIO
  putStr "Please enter how many years ago to get records (0 for current):"
  idx <- getIntIO
  case idx of
    (Just i) -> printf "Population: %s\n\n" (getPopulation cs city i)
    Nothing -> putStrLn $ red "Invalid input.\n"

doAnnualGrowthIO :: [City] -> IO ()
doAnnualGrowthIO cs = do
  nm <- getCityNameIO
  let growth = unlines $ printf "• %.2f%%" <$> getGrowth cs nm
  if null growth
    then putStrLn "No data available.\n"
    else putStrLn $ underline "Annual Growth Figures:\n\n" ++ growth

dosortedInsertIO :: [City] -> IO [City]
dosortedInsertIO cs = do
  nm <- getCityNameIO
  putStr "Please enter degrees north:\n> "
  nrth <- getIntIO
  putStr "Please enter degrees east:\n> "
  est <- getIntIO
  putStrLn "Please enter population figures (from most recent, 2+ entries)"
  pops <- getListOfIntsIO
  case (nrth, est) of
    (Just n, Just e) -> return $ cs `sortedInsert` City nm n e pops
    _ -> putStrLn (red "Invalid data given, not adding city\n") >> return cs
