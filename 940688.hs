import Data.List (elemIndex) -- For finding index of elements
import Text.Printf (printf) -- For string formatting
import Text.Read (readMaybe) -- For parsing IO String -> IO (Maybe a)

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

cityFromName :: [City] -> Name -> Maybe City
cityFromName cities = fmap (cities !!) . (`elemIndex` getNames cities)

fmtRecord :: Population -> FormattedPopulation
fmtRecord = printf "%.3fm" . (/ 1000) . toFloat

-- function composition that passes two values instead of one
-- i.e. a = f .: g is equivalent to a = (f .) . g
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

------------------------------------------------
--          core functionality (i)            --
------------------------------------------------

getNames :: [City] -> [Name]
getNames = map getName

------------------------------------------------
--           core functionality (ii)          --
------------------------------------------------

getPopulation :: [City] -> Name -> Index -> FormattedPopulation
getPopulation = maybe "no data" fmtRecord .: maybeRecord .: cityFromName

maybeRecord :: Maybe City -> Index -> Maybe Population
maybeRecord (Just (City _ _ records)) i
  | i >= 0 && i < length records = Just (records !! i)
  | otherwise = Nothing
maybeRecord Nothing _ = Nothing

------------------------------------------------
--         core functionality (iii)           --
------------------------------------------------

citiesToString :: [City] -> OutString
citiesToString = tableFormat . concatMap cityRow

cityRow :: City -> OutString
cityRow (City name (n, e) (x:y:_)) = printf
  "| %-13s | %10d | %10d | %10s | %10s |\n" name n e (fmtRecord x) (fmtRecord y)

tableFormat :: OutString -> OutString
tableFormat text = printf "%s| Name          | Deg. North |  Deg. East \
   \| Population |  Last Year |\n%s%s%s" line line text line

line :: OutString
line = "+---------------+------------+------------+------------+------------+\n"

------------------------------------------------
--         core functionality (iv)            --
------------------------------------------------

updateRecords :: [City] -> [Population] -> [City]
cities `updateRecords` pops = changed ++ drop (length changed) cities
  where changed = zipWith addYear cities pops

addYear :: City -> Population -> City
addYear (City name loc records) pop = City name loc (pop : records)

------------------------------------------------
--           core functionality (v)           --
------------------------------------------------

insert :: (Ord a) => [a] -> a -> [a]
xs `insert` x = lower ++ (x : higher)
  where (lower, higher) = span (< x) xs

------------------------------------------------
--         core functionality (vi)            --
------------------------------------------------

cityGrowth :: [City] -> Name -> [Growth]
cityGrowth = maybe [] mapGrowth .: cityFromName

mapGrowth :: City -> [Growth]
-- (zip <*> tail) xs == zip xs (tail xs)
mapGrowth = map growth . (zip <*> tail) . getRecords

growth :: (Population, Population) -> Growth
growth (p1, p2) = toFloat (p1 - p2) / toFloat p1 * 100

------------------------------------------------
--         core functionality (vii)           --
------------------------------------------------

nearestCity :: [City] -> Location -> Population -> Name
nearestCity cs loc pop = if null pairs then "no data" else snd (minimum pairs)
  where
    pairs = map (distancePair loc) filtered
    filtered = filter ((pop < ) . head . getRecords) cs

distancePair :: Location -> City -> (Distance, Name)
distancePair loc (City name loc2 _) = (distance loc loc2, name)

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
demo 7 = putStrLn (nearestCity testData (54, 6) 2000)
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

drawCities :: [City] -> IO ()
drawCities cities = do
  clearScreen
  plots <- mapM drawCity cities
  goTo (0, maximum (0:plots) + 3) -- go to three lines below lowest drawn city

drawCity :: City -> IO Int
drawCity (City name loc records) = do
  let (x, y) = locationToPosition loc 
  writeAt (x, y) ("+ " ++ name)
  writeAt (x, y + 1) $ (fmtRecord . head) records
  return (y + 1)

locationToPosition :: Location -> ScreenPosition
locationToPosition (n, e) = (e * 2, abs (n - 54) * 2)

------------------------------------------------
--               user interface               --
------------------------------------------------

main :: IO ()
main = do
  file <- readFile "cities.txt"
  let cities = read file :: [City]
  putStrLn (getPrettyNamesString cities)
  updatedCities <- loopChoices cities
  writeFile "cities.txt" updatedCities

options :: OutString
options = "\nOptions:\n\n\
  \(1) Show names \n\
  \(2) Return population of city n years ago \n\
  \(3) Display cities data \n\
  \(4) Update data to include new population figures \n\
  \(5) Add a new city to the data \n\
  \(6) Get annual population growth percentage figs for a city \n\
  \(7) Locate nearest city to given point above given population \n\
  \(8) Draw city map \n\
  \(9) Exit"

promptUser :: OutString -> IO String
promptUser str = do
  putStr (str ++ "\n\n>>> ")
  ln <- getLine
  putStr "\n"
  return ln

loopChoices :: [City] -> IO String
loopChoices cities = do
  choice <- promptUser options
  if choice == "9"
    then return (show cities)
    else loopChoices =<< case choice of
      "1" -> putStrLn (getPrettyNamesString cities) >> return cities
      "2" -> doPopulationIO cities >> return cities
      "3" -> putStr (citiesToString cities) >> return cities
      "4" -> putStr . citiesToString <$> updatedRecs >> updatedRecs
      "5" -> putStr . citiesToString <$> insertedCity >> insertedCity
      "6" -> doAnnualGrowthIO cities >> return cities
      "7" -> doFindCityIO cities >> return cities
      "8" -> drawCities cities >> return cities
      _  -> putStrLn "Invalid choice." >> return cities
      where
        updatedRecs = updateRecords cities <$> getListOfIntsIO
        insertedCity = doInsertIO cities

getPrettyNamesString :: [City] -> OutString
getPrettyNamesString = ("City Names:\n" ++) . concatMap (("\n* "++) . getName)

getCityNameIO :: IO Name
getCityNameIO = promptUser "Please enter city name:"

getIntIO :: String -> IO (Maybe Int)
getIntIO = fmap readMaybe . promptUser

getLocationIO :: String -> IO (Maybe Location)
getLocationIO = fmap readMaybe . promptUser

getListOfIntsIO :: IO [Int]
getListOfIntsIO = do
  input <- getIntIO "Please enter an integer (non-integer to finish)"
  maybe (return []) (\int -> (int :) <$> getListOfIntsIO) input

doFindCityIO :: [City] -> IO ()
doFindCityIO cities = do
  loc <- getLocationIO "Please enter city's location in form (N, E):"
  pop <- getIntIO "Please enter minimum population city should have:"
  putStrLn $ case (loc, pop) of
    (Just l, Just p) -> "Nearest City: " ++ nearestCity cities l p
    _ -> "Invalid population figure entered."

doPopulationIO :: [City] -> IO ()
doPopulationIO cities = do
  name <- getCityNameIO
  idx <- getIntIO "Please enter how many years ago to get records (0 = current)"
  putStrLn $ maybe "Please enter valid integer." (populationOf name) idx
  where populationOf = ("Population: " ++) .: getPopulation cities

doAnnualGrowthIO :: [City] -> IO ()
doAnnualGrowthIO cities = do
  name <- getCityNameIO
  let growths = printf "\n* %.2f%%" =<< cityGrowth cities name
  putStrLn $ if null growths then "No data available." else annualFigs growths
  where annualFigs growths = "Annual Growth Figures:\n" ++ growths

doInsertIO :: [City] -> IO [City]
doInsertIO cities = do
  name <- getCityNameIO
  maybeLoc <- getLocationIO "Please enter city's location in form (N, E):"
  putStrLn "Please enter population figures (from most recent, 2+ entries)"
  pops <- getListOfIntsIO
  maybe (err "Invalid data, not adding city." cities) (\loc ->
      if length pops >= 2
        then return (cities `insert` City name loc pops)
        else err "Not enough population figures, not adding city." cities
    ) maybeLoc
  where err str c = putStrLn str >> return c
