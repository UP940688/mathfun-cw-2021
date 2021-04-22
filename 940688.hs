-- For finding index of elements and table formatting
import Data.List (elemIndex, intercalate)
-- For filtering [Maybe a] -> [a]
import Data.Maybe (catMaybes)
-- For string formatting
import Text.Printf (printf)
-- For parsing IO String -> IO (Maybe a)
import Text.Read (readMaybe)

------------------------------------------------
--            types + test data               --
------------------------------------------------

testData :: [City]
testData =
  [ City "Amsterdam" (52, 5) [1158, 1149, 1140, 1132],
    City "Athens" (38, 23) [3153, 3153, 3154, 3156],
    City "Berlin" (53, 13) [3567, 3562, 3557, 3552],
    City "Brussels" (51, 4) [2096, 2081, 2065, 2050],
    City "Bucharest" (44, 26) [1794, 1803, 1812, 1821],
    City "London" (52, 0) [9426, 9304, 9177, 9046],
    City "Madrid" (40, 4) [6669, 6618, 6559, 6497],
    City "Paris" (49, 2) [11079, 11017, 10958, 10901],
    City "Rome" (42, 13) [4278, 4257, 4234, 4210],
    City "Sofia" (43, 23) [1284, 1281, 1277, 1272],
    City "Vienna" (48, 16) [1945, 1930, 1915, 1901],
    City "Warsaw" (52, 21) [1790, 1783, 1776, 1768]
  ]

data City = City
  { getName :: Name,
    getLocation :: Location,
    getRecords :: [Population]
  }
  deriving (Eq, Ord, Show, Read)

type Location = (Int, Int)

type Population = Int

type Index = Int

type Distance = Float

type Growth = Float

type FormattedPopulation = String

type Name = String

type OutString = String

------------------------------------------------
--              helper functions              --
------------------------------------------------

toFloat :: Integral a => a -> Float
toFloat = fromIntegral

-- | Returns (Just City) if given valid name, Nothing otherwise.
cityFromName :: [City] -> Name -> Maybe City
cityFromName cities = fmap (cities !!) . (`elemIndex` getNames cities)

fmtRecord :: Population -> FormattedPopulation
fmtRecord = printf "%.3fm" . (/ 1000) . toFloat

------------------------------------------------
--          core functionality (i)            --
------------------------------------------------

getNames :: [City] -> [Name]
getNames = map getName

------------------------------------------------
--           core functionality (ii)          --
------------------------------------------------

getPopulation :: [City] -> Name -> Index -> FormattedPopulation
getPopulation = (populationAt .) .  cityFromName

maybeRecord :: Index -> City -> Maybe Population
maybeRecord i (City _ _ records)
  | i >= 0 && i < length records = Just (records !! i)
  | otherwise = Nothing

-- | Returns FormattedPopulation of (Maybe City) from Index years ago.
populationAt :: Maybe City -> Index -> FormattedPopulation
populationAt city = maybe "no data" fmtRecord . (city >>=) . maybeRecord

------------------------------------------------
--         core functionality (iii)           --
------------------------------------------------

-- TODO: may need to change output if no cities

citiesToString :: [City] -> OutString
citiesToString = wrap . concatMap cityRow

cityRow :: City -> OutString
cityRow (City nm (n, e) (x:y:_)) = printf
  "| %-16s | %10d | %10d | %10s | %10s |\n" nm n e (fmtRecord x) (fmtRecord y)

wrap :: String -> OutString
wrap text = (line ++ header ++ line) ++ text ++ line

header :: OutString
header = 
  "| Name             | Deg. North |  Deg. East | Population |  Last Year |\n"

line :: OutString
line = "+------" ++ intercalate "+" (replicate 5 "------------") ++ "+\n"

------------------------------------------------
--         core functionality (iv)            --
------------------------------------------------

-- | Given a list of cities and list of population records, return
-- an updated list of cities reflecting the new data.
updateRecords :: [City] -> [Population] -> [City]
cities `updateRecords` pops = changed ++ drop (length changed) cities
  where changed = zipWith addYear cities pops

addYear :: City -> Population -> City
addYear (City name loc records) pop = City name loc (pop : records)

------------------------------------------------
--           core functionality (v)           --
------------------------------------------------

-- may want to perform checks on what
-- can be inserted (name not null, num of records)

insert :: (Ord a) => [a] -> a -> [a]
xs `insert` x = lower ++ (x : higher)
  where (lower, higher) = span (< x) xs

------------------------------------------------
--         core functionality (vi)            --
------------------------------------------------

cityGrowth :: [City] -> Name -> [Growth]
cityGrowth cities = maybe [] mapGrowth . cityFromName cities

mapGrowth :: City -> [Growth]
-- (zip <*> tail) xs == zip xs (tail xs)
mapGrowth = map growth . (zip <*> tail) . getRecords

growth :: (Population, Population) -> Growth
growth (p1, p2) = toFloat (p1 - p2) / toFloat p1 * 100

------------------------------------------------
--         core functionality (vii)           --
------------------------------------------------

-- this might be broken

nearestCityName :: [City] -> Location -> Population -> Name
nearestCityName cities = (maybe "no data" getName .) . nearestCity cities

-- | Return (Just City) with the smallest calculated distance to provided
-- Location, with a population above the limit provided. Will return Nothing
-- if no suitable candidates can be found.
nearestCity :: [City] -> Location -> Population -> Maybe City
nearestCity cities loc pop = (cities !!) <$> elemIndex (minimum dists) dists
  where
    dists = map (distance loc . getLocation) candidates
    candidates = filter ((pop <) . head . getRecords) cities

distance :: Location -> Location -> Distance
distance (x, y) (x2, y2) = sqrt . toFloat $ (x - x2) ^ 2 + (y - y2) ^ 2

------------------------------------------------
--               demo execution               --
------------------------------------------------

demo :: Int -> IO ()
demo 1 = print (getNames testData)
demo 2 = putStrLn (getPopulation testData "Madrid" 2)
demo 3 = putStr (citiesToString testData)
demo 4 = putStr . citiesToString $ updateRecords testData
  [1200, 3200, 3600, 2100, 1800, 9500, 6700, 11100, 4300, 1300, 2000, 1800]
demo 5 = putStr . citiesToString $
  testData `insert` City "Prague" (50, 14) [1312, 1306, 1299, 1292]
demo 6 = print (cityGrowth testData "London")
demo 7 = putStrLn (nearestCityName testData (54, 6) 2000)
demo 8 = drawCities testData
demo _ = putStrLn "Please pick a number 1-8."

------------------------------------------------
--                  screen utils              --
------------------------------------------------

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

------------------------------------------------
--              population map                --
------------------------------------------------

-- | Clear the screen, plot a list of cities to the screen, then adjust cursor.
drawCities :: [City] -> IO ()
drawCities cities = clearScreen >> mapM drawCity cities >>= adjustCursor

-- | Move the cursor to 3 lines below the lowest Y axis point passed.
adjustCursor :: [Int] -> IO ()
adjustCursor ys = goTo (0, maximum ys + 3)

drawCity :: City -> IO Int
drawCity (City name loc records) = do
  writeAt (x, y) ("+ " ++ name)
  writeAt (x, y + 1) $ (fmtRecord . head) records
  return (y + 1)
  where (x, y) = locationToPosition loc

locationToPosition :: Location -> ScreenPosition
locationToPosition (n, e) = (e * 2, abs (n - 54) * 2)

------------------------------------------------
--               user interface               --
------------------------------------------------

main :: IO ()
main = do
  tmp <- fileToCities "cities.txt"
  let cities = catMaybes tmp
      (lenT, lenC) = (length tmp, length cities)
  -- notify the user how many have been filtered out
  printf "\nINFO: Loaded %i/%i cities.\n\n" lenC lenT
  putStrLn (getPrettyNamesString cities)
  updatedCities <- loopChoices cities
  writeFile "cities.txt" updatedCities

-- fmap to map over IO Monad, then map (readMaybe String :: Maybe City)
fileToCities :: FilePath -> IO [Maybe City]
fileToCities fp = map readMaybe . lines <$> readFile fp

showOptions :: IO ()
showOptions = putStr $ "\nOptions:" ++
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
  putStr "\n\n>>> "
  getLine >>= (\ ln -> putStrLn "\ESC[0m" >> return ln)

loopChoices :: [City] -> IO String
loopChoices cities = do
  showOptions
  choice <- promptUser
  -- split into two cases to reduce complexity of handling
  -- options that don't alter city data
  updatedCities <- case choice of
    "4" -> do
      new <- updateRecords cities <$> getListOfIntsIO
      putStr (citiesToString new)
      return new
    "5" -> do
      new <- doinsertIO cities
      putStr (citiesToString new)
      return new
    _ -> matchChoice cities choice >> return cities

  if choice == "9"
    then return (unlines $ map show updatedCities)
    else loopChoices updatedCities

matchChoice :: [City] -> String -> IO ()
matchChoice cities choice
  | choice == "1" = putStrLn $ getPrettyNamesString cities
  | choice == "2" = doPopulationIO cities
  | choice == "3" = putStr $ citiesToString cities
  | choice == "6" = doAnnualGrowthIO cities
  | choice == "7" = doFindCityIO cities
  | choice == "8" = drawCities cities
  | choice `elem` ["4", "5", "9"] = return ()
  | otherwise = putStrLn "Invalid choice."

getPrettyNamesString :: [City] -> String
getPrettyNamesString = ("City Names:\n" ++) . nameList
  where nameList = concatMap (printf "\n• %s" . getName)

promptInput :: String -> IO String
promptInput str = putStr str >> promptUser

getCityNameIO :: IO Name
getCityNameIO = promptInput "Please enter city name:"

getIntIO :: String -> IO (Maybe Int)
getIntIO = fmap readMaybe . promptInput

getLocationIO :: String -> IO (Maybe Location)
getLocationIO = fmap readMaybe . promptInput

getListOfIntsIO :: IO [Int]
getListOfIntsIO = do
  input <- getIntIO "Please enter an integer (non-integer to finish)"
  maybe (return []) (\ int -> (int :) <$> getListOfIntsIO) input

doFindCityIO :: [City] -> IO ()
doFindCityIO cities = do
  loc <- getLocationIO "Please enter city's location in form (N, E):"
  pop <- getIntIO "Please enter minimum population city should have:"
  putStrLn $ case (loc, pop) of
    (Just l, Just p) -> "Nearest City: " ++ nearestCityName cities l p
    _ -> "Invalid population figure entered."

doPopulationIO :: [City] -> IO ()
doPopulationIO cities = do
  name <- getCityNameIO
  idx <- getIntIO "Please enter how many years ago to get records (0 = current)"
  putStrLn $ maybe "Please enter valid integer." (populationOf name) idx
  where populationOf name = ("Population: " ++) . getPopulation cities name

doAnnualGrowthIO :: [City] -> IO ()
doAnnualGrowthIO cities = do
  name <- getCityNameIO
  let growths = concatMap (printf "\n• %.2f%%") (cityGrowth cities name)
  putStrLn $ if null growths
    then "No data available."
    else "Annual Growth Figures:\n" ++ growths

doinsertIO :: [City] -> IO [City]
doinsertIO cities = do
  name <- getCityNameIO
  maybeLoc <- getLocationIO "Please enter city's location in form (N, E):"
  putStrLn "Please enter population figures (from most recent, 2+ entries)"
  pops <- getListOfIntsIO
  maybe (err "Invalid data, not adding city." cities) (\ loc ->
      if length pops >= 2
        then return (cities `insert` City name loc pops)
        else err "Not enough population figures, not adding city." cities
    ) maybeLoc
  where err str c = putStrLn str >> return c
