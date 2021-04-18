-- for applying a value to 2 functions, then applying those to another function
import Control.Monad (liftM2)
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
  { name :: Name,
    north :: Int,
    east :: Int,
    records :: [Population]
  }
  deriving (Eq, Ord, Show, Read)

-- below are defined for easier reading of type signatures

type Location = (Int, Int)

type Population = Int

type Index = Int

type Distance = Float

type Growth = Float

type FormattedPopulation = String

type Name = String

--------------------------------------------------------
--                  helper functions                  --
--------------------------------------------------------

toFloat :: Integral a => a -> Float
toFloat = fromIntegral

-- applies city to north + east, then joins result into a pair
location :: City -> Location
location = liftM2 (,) north east

cityFromName :: [City] -> Name -> Maybe City
cityFromName cities nm = Just (cities !!) <*> elemIndex nm (getNames cities)

fmtPopulation :: Population -> FormattedPopulation
fmtPopulation = printf "%.3fm" . (/ 1000) . toFloat

------------------------------------------------
--                  section one               --
------------------------------------------------

getNames :: [City] -> [Name]
getNames = map name

------------------------------------------------
--                  section two               --
------------------------------------------------

getPopulation :: [City] -> Name -> Index -> FormattedPopulation
getPopulation = fmap populationAt . cityFromName

maybeRecord :: City -> Index -> Maybe Population
maybeRecord city i
  | i >= 0 && i < (length . records) city = Just (records city !! i)
  | otherwise = Nothing

populationAt :: Maybe City -> Int -> FormattedPopulation
(Just city) `populationAt` i = maybe "no data" fmtPopulation (maybeRecord city i)
Nothing `populationAt` _ = "no data"

--------------------------------------------------
--                  section three               --
--------------------------------------------------

citiesToString :: [City] -> String
citiesToString = wrap columnHeader columnLine . concatMap cityRow

cityRow :: City -> String
cityRow city = printf "| %-12s | %12d | %12d | %12s | %12s |\n"
  (name city) (north city) (east city) cur pvs
  where
    [cur, pvs] = (take 2 . map fmtPopulation . records) city

wrap :: [a] -> [a] -> [a] -> [a]
wrap headr footr mid = headr ++ mid ++ footr

columnHeader :: String
columnHeader = wrap columnLine columnLine
  "|     Name     |  Deg. North  |   Deg. East  |  Population  |   Previous   |\n"

columnLine :: String
columnLine = wrap "+" "+\n" $ intercalate "+" (replicate 5 "--------------")

-------------------------------------------------
--                  section four               --
-------------------------------------------------

updateRecords :: [City] -> [Population] -> [City]
cities `updateRecords` pops = changed ++ unchanged
  where
    changed = zipWith addYearToRecord cities pops
    unchanged = drop (length changed) cities

addYearToRecord :: City -> Population -> City
addYearToRecord c pop = City (name c) (north c) (east c) (pop : records c)

-------------------------------------------------
--                  section five               --
-------------------------------------------------

insert :: (Ord a) => [a] -> a -> [a]
xs `insert` x = lower ++ (x : higher)
  where (lower, higher) = span (< x) xs

------------------------------------------------
--                  section six               --
------------------------------------------------

cityGrowth :: [City] -> Name -> [Growth]
cityGrowth = fmap (maybe [] mapGrowth) . cityFromName

-- (zip <*> tail) == zip xs (tail xs)
mapGrowth :: City -> [Growth]
mapGrowth = map growth . (zip <*> tail) . records

growth :: (Population, Population) -> Growth
growth (p1, p2) = toFloat (p1 - p2) / toFloat p1 * 100

--------------------------------------------------
--                  section seven               --
--------------------------------------------------

nearestCityName :: [City] -> Location -> Population -> Name
nearestCityName cities = fmap (maybe "no data" name) . nearestCity cities

-- get city with the lowest distance score to point (match up indexes)
nearestCity :: [City] -> Location -> Population -> Maybe City
nearestCity cities loc pop = Just (cities !!) <*> minIndex candidates
  where
    minIndex = (elemIndex =<< minimum) . map (distance loc . location)
    candidates = filter (\c -> head (records c) > pop) cities

distance :: Location -> Location -> Distance
distance (x1, y1) (x2, y2) = sqrt . toFloat $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

------------------------------------------------------
--                  demo execution                  --
------------------------------------------------------

demo :: Int -> IO ()
demo 1 = print (getNames testData)
demo 2 = putStrLn (getPopulation testData "Madrid" 2)
demo 3 = putStr (citiesToString testData)
demo 4 = putStr . citiesToString $ updateRecords testData
  [1200, 3200, 3600, 2100, 1800, 9500, 6700, 11100, 4300, 1300, 2000, 1800]
demo 5 = putStr . citiesToString $
  testData `insert` City "Prague" 50 14 [1312, 1306, 1299, 1292]
demo 6 = print (cityGrowth testData "London")
demo 7 = putStrLn (nearestCityName testData (54, 6) 2000)
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

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
  goTo position
  putStr text

------------------------------------------------------
--                  population map                  --
------------------------------------------------------

-- IO is a monad, so use mapM to apply drawCity to each city
-- then pass a list of mapped y co-ordinates to adjustCursor
drawCities :: [City] -> IO ()
drawCities cities = clearScreen >> mapM drawCity cities >>= adjustCursor

adjustCursor :: [Int] -> IO ()
adjustCursor positions = goTo (0, maximum positions + 4) -- 4 below lowest y

drawCity :: City -> IO Int
drawCity city = drawCityPlot (location city) (name city) (head $ records city)

drawCityPlot :: Location -> Name -> Population -> IO Int
drawCityPlot loc nm pop = do
  let (x, y) = locationToPosition loc
  writeAt (x, y) ("+ " ++ nm)
  writeAt (x, y + 1) (fmtPopulation pop) -- write population underneath name
  return y

-- flip y axis, multiply by 2 to increase distance between cities
locationToPosition :: Location -> ScreenPosition
locationToPosition (n, e) = (e * 2, abs (n - 54) * 2)

------------------------------------------------------
--                  user interface                  --
------------------------------------------------------

main :: IO ()
main = do
  tmp <- fileToCities "cities.txt"
  let cities = catMaybes tmp
      (tmpLen, citiesLen) = (length tmp, length cities)
  -- notify the user how many have been filtered out
  loaded <- case tmpLen - citiesLen of
    0 -> return $ green $ printf "%i/%i" citiesLen tmpLen
    _ -> return $ red $ printf "%i/%i" citiesLen tmpLen
  printf "\nINFO: Loaded %s cities.\n" loaded
  putStrLn $ '\n' : getPrettyNamesString cities
  updatedCities <- loopChoices cities
  writeFile "cities.txt" updatedCities

-- fmap to map over IO monad, map to map over the actual String
fileToCities :: FilePath -> IO [Maybe City]
fileToCities = fmap (map readMaybe . lines) . readFile

showOptions :: IO ()
showOptions = putStr $ underline "Options:" ++
  "\n\n\
  \(1) Show names \n\
  \(2) Return population of city n years ago \n\
  \(3) Display cities data \n\
  \(4) Update data to include new population figures \n\
  \(5) Add a new city to the data \n\
  \(6) Get annual population growth percentage figs for a city \n\
  \(7) Locate nearest city to given point above given population \n\
  \(8) Draw city map \n\
  \(9) Exit"

promptUser :: IO String
promptUser = do
  putStr "\n\n\ESC[1;2m>>>\ESC[0;2m "
  getLine >>= (\ln -> putStrLn endFmt >> return ln)

underline :: String -> String
underline = ("\ESC[1;4m" ++) . (++ endFmt)

red :: String -> String
red = ("\ESC[31m" ++) . (++ endFmt)

green :: String -> String
green = ("\ESC[32m" ++) . (++ endFmt)

endFmt :: String
endFmt = "\ESC[0m"

loopChoices :: [City] -> IO String
loopChoices cities = do
  showOptions
  choice <- promptUser
  -- split into two cases to reduce complexity of handling
  -- options that don't alter city data
  updatedCities <- case choice of
    "4" -> do
      new <- fmap (updateRecords cities) getListOfIntsIO
      putStrLn $ citiesToString new
      return new
    "5" -> do
      new <- doinsertIO cities
      putStrLn $ citiesToString new
      return new
    _ -> matchChoice cities choice >> return cities

  if choice == "9"
    then return (unlines $ map show updatedCities)
    else loopChoices updatedCities

matchChoice :: [City] -> String -> IO ()
matchChoice cities choice
  | choice == "1" = putStrLn $ getPrettyNamesString cities
  | choice == "2" = doPopulationIO cities
  | choice == "3" = putStrLn $ citiesToString cities
  | choice == "6" = doAnnualGrowthIO cities
  | choice == "7" = doFindCityIO cities
  | choice == "8" = drawCities cities
  | choice `elem` ["4", "5", "9"] = return ()
  | otherwise = putStrLn $ red "Invalid choice.\n"

getPrettyNamesString :: [City] -> String
getPrettyNamesString =
  (underline "City Names:\n\n" ++)
    . unlines
    . map (("• " ++) . name)

getCityNameIO :: IO Name
getCityNameIO = putStr "Please enter city name:" >> promptUser

getIntIO :: String -> IO (Maybe Int)
getIntIO str = putStr str >> readMaybe <$> promptUser

getListOfIntsIO :: IO [Int]
getListOfIntsIO = do
  input <- getIntIO "Please enter an integer (non-integer to finish)"
  maybe (return []) getList input
  where
    getList x = fmap (x :) getListOfIntsIO

doFindCityIO :: [City] -> IO ()
doFindCityIO cities = do
  dNorth <- getIntIO "Please enter city's location (degrees north):"
  dEast <- getIntIO "Please enter city's location (degrees east):"
  pop <- getIntIO "Please enter minimum population city should have:"
  case (dNorth, dEast, pop) of
    (Just n, Just e, Just p) ->
      printf "Nearest City: %s\n\n" (nearestCityName cities (n, e) p)
    _ -> putStrLn $ red "Invalid population figure entered."

doPopulationIO :: [City] -> IO ()
doPopulationIO cities = do
  city <- getCityNameIO
  idx <- getIntIO "Please enter how many years ago to get records (0 for current):"
  case idx of
    (Just i) -> printf "Population: %s\n\n" (getPopulation cities city i)
    Nothing -> putStrLn $ red "Please enter a valid integer.\n"

doAnnualGrowthIO :: [City] -> IO ()
doAnnualGrowthIO cities = do
  nm <- getCityNameIO
  let growths = unlines $ printf "• %.2f%%" <$> cityGrowth cities nm
  if null growths
    then putStrLn "No data available.\n"
    else putStrLn $ underline "Annual Growth Figures:\n\n" ++ growths

doinsertIO :: [City] -> IO [City]
doinsertIO cities = do
  nm <- getCityNameIO
  dNorth <- getIntIO "Please enter degrees north:"
  dEast <- getIntIO "Please enter degrees east:"
  putStrLn "Please enter population figures (from most recent, 2+ entries)"
  pops <- getListOfIntsIO
  case (dNorth, dEast) of
    (Just n, Just e)
      | length pops >= 2 -> return (cities `insert` City nm n e pops)
      | otherwise -> do
        putStrLn $ red "Not enough population figures, not adding city.\n"
        return cities
    _ -> putStrLn (red "Invalid data given, not adding city.\n") >> return cities
