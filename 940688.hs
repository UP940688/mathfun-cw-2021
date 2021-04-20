-- for finding index of elements and table formatting
import Data.List (elemIndex, intercalate)
-- for filtering [Maybe a] -> [a]
import Data.Maybe (catMaybes)
-- for string formatting
import Text.Printf (printf)
-- for parsing IO String -> IO (Maybe a)
import Text.Read (readMaybe)

------------------------------------------------
--                   types                    --
------------------------------------------------

testData :: [City]
testData =
  [ City "Amsterdam" (Loc 52 5) [1158, 1149, 1140, 1132],
    City "Athens" (Loc 38 23) [3153, 3153, 3154, 3156],
    City "Berlin" (Loc 53 13) [3567, 3562, 3557, 3552],
    City "Brussels" (Loc 51 4) [2096, 2081, 2065, 2050],
    City "Bucharest" (Loc 44 26) [1794, 1803, 1812, 1821],
    City "London" (Loc 52 0) [9426, 9304, 9177, 9046],
    City "Madrid" (Loc 40 4) [6669, 6618, 6559, 6497],
    City "Paris" (Loc 49 2) [11079, 11017, 10958, 10901],
    City "Rome" (Loc 42 13) [4278, 4257, 4234, 4210],
    City "Sofia" (Loc 43 23) [1284, 1281, 1277, 1272],
    City "Vienna" (Loc 48 16) [1945, 1930, 1915, 1901],
    City "Warsaw" (Loc 52 21) [1790, 1783, 1776, 1768]
  ]

data City = City
  { getName :: Name,
    getLocation :: Location,
    getRecords :: [Population]
  }
  deriving (Eq, Ord, Show, Read)

data Location = Loc Int Int deriving (Eq, Ord, Show, Read)

-- below are defined for easier reading of type signatures

type Population = Int

type Index = Int

type Distance = Float

type Growth = Float

type FormattedPopulation = String

type Name = String

--------------------------------------------------------
--                  helper functions                  --
--------------------------------------------------------

-- allows us to use (toFloat x) rather than (fromIntegral x :: Float)
toFloat :: Integral a => a -> Float
toFloat = fromIntegral

-- f (a -> b) <*> f (a) = f (b) (beefed up map / fmap)
cityFromName :: [City] -> Name -> Maybe City
cityFromName cities name = Just (cities !!) <*> elemIndex name (getNames cities)

fmtPopulation :: Population -> FormattedPopulation
fmtPopulation = printf "%.3fm" . (/ 1000) . toFloat

------------------------------------------------
--                  section one               --
------------------------------------------------

getNames :: [City] -> [Name]
getNames = map getName

------------------------------------------------
--                  section two               --
------------------------------------------------

getPopulation :: [City] -> Name -> Index -> FormattedPopulation
getPopulation = fmap populationAt . cityFromName

maybeRecord :: City -> Index -> Maybe Population
maybeRecord (City _ _ records) i
  | i >= 0 && i < length records = Just (records !! i)
  | otherwise = Nothing

-- maybe takes default b, function (a -> b), and (Maybe a)
-- if Nothing, return default, if (Just a), return result of (a -> b)
populationAt :: Maybe City -> Index -> FormattedPopulation
populationAt (Just city) i = maybe "no data" fmtPopulation (maybeRecord city i)
populationAt Nothing _ = "no data"

--------------------------------------------------
--                  section three               --
--------------------------------------------------

citiesToString :: [City] -> String
citiesToString = wrap . concatMap cityRow

cityRow :: City -> String
cityRow (City name (Loc n e) records) =
  printf "| %-12s | %12d | %12d | %12s | %12s |\n" name n e cur pvs
  -- take advantage of haskell's laziness -- it will only map 2 elements
  where [cur, pvs] = (take 2 . map fmtPopulation) records

wrap :: String -> String
wrap text = (line ++ header ++ line) ++ text ++ line

header :: String
header = "|     Name     |  Deg. North  |   Deg. East  |  Population  |   Previous   |\n"

-- intercalate :: [a] -> [[a]] -> [a] (joins [[a]] with first argument)
line :: String
line = '+' : intercalate "+" (replicate 5 "--------------") ++ "+\n"

-------------------------------------------------
--                  section four               --
-------------------------------------------------

updateRecords :: [City] -> [Population] -> [City]
cities `updateRecords` pops = changed ++ unchanged
  where
    changed = zipWith addYear cities pops
    unchanged = drop (length changed) cities

addYear :: City -> Population -> City
addYear (City name loc records) pop = City name loc (pop : records)

-------------------------------------------------
--                  section five               --
-------------------------------------------------

insert :: (Ord a) => [a] -> a -> [a]
xs `insert` x = lower ++ (x : higher)
  where (lower, higher) = span (< x) xs

------------------------------------------------
--                  section six               --
------------------------------------------------

-- <$> = infixr fmap (map for all functors), where (a -> b) <$> f (a) = f (b)
cityGrowth :: [City] -> Name -> [Growth]
cityGrowth cities = maybe [] mapGrowth <$> cityFromName cities

-- (zip <*> tail) xs == zip xs (tail xs), convenient for pointfree notation
mapGrowth :: City -> [Growth]
mapGrowth = map growth . (zip <*> tail) . getRecords

growth :: (Population, Population) -> Growth
growth (p1, p2) = toFloat (p1 - p2) / toFloat p1 * 100

--------------------------------------------------
--                  section seven               --
--------------------------------------------------

nearestCityName :: [City] -> Location -> Population -> Name
nearestCityName cities = fmap (maybe "no data" getName) . nearestCity cities

nearestCity :: [City] -> Location -> Population -> Maybe City
nearestCity cities loc pop = Just (cities !!) <*> minIndex candidates
  where
    -- y =<< x is equivalent to (x >>= (\a -> y a)) (feeds result of x to y)
    minIndex = (elemIndex =<< minimum) . map (distance loc . getLocation)
    candidates = filter (\ (City _ _ pops) -> head pops > pop) cities

distance :: Location -> Location -> Distance
distance (Loc x y) (Loc x2 y2) = sqrt . toFloat $ (x - x2) ^ 2 + (y - y2) ^ 2

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
  testData `insert` City "Prague" (Loc 50 14) [1312, 1306, 1299, 1292]
demo 6 = print (cityGrowth testData "London")
demo 7 = putStrLn (nearestCityName testData (Loc 54 6) 2000)
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

-- IO is a Monad, so use mapM(onad) to apply drawCity to each city
-- x >> y roughly means 'do x, then y'
drawCities :: [City] -> IO ()
drawCities cities = clearScreen >> mapM drawCity cities >>= adjustCursor

adjustCursor :: [Int] -> IO ()
adjustCursor ys = goTo (0, maximum ys + 4) -- 4 below lowest mapped city

drawCity :: City -> IO Int
drawCity (City name loc records) = do
  writeAt (x, y) ("+ " ++ name)
  -- x $ f a b is equivalent to x (f a b)
  writeAt (x, y + 1) $ (fmtPopulation . head) records
  return y
  where (x, y) = locationToPosition loc

-- flip y axis, multiply by 2 to increase distance between cities
locationToPosition :: Location -> ScreenPosition
locationToPosition (Loc n e) = (e * 2, abs (n - 54) * 2)

------------------------------------------------------
--                  user interface                  --
------------------------------------------------------

main :: IO ()
main = do
  tmp <- fileToCities "cities.txt"
  let cities = catMaybes tmp
      (len1, len2) = (length tmp, length cities)
      colour = if len1 - len2 == 0 then green else red
  -- notify the user how many have been filtered out
  printf "\nINFO: Loaded %s cities.\n" $ colour (printf "%i/%i" len2 len1)
  putStrLn ('\n' : getPrettyNamesString cities)
  updatedCities <- loopChoices cities
  writeFile "cities.txt" updatedCities

-- fmap to map over IO Monad, then map (readMaybe String :: Maybe City)
fileToCities :: FilePath -> IO [Maybe City]
fileToCities fp = map readMaybe . lines <$> readFile fp

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
  getLine >>= (\ ln -> putStrLn endFmt >> return ln)

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
  | otherwise = putStrLn (red "Invalid choice.\n")

getPrettyNamesString :: [City] -> String
getPrettyNamesString = (underline "City Names:\n\n" ++) . nameList
  where nameList = concatMap (\ (City name _ _) -> printf "• %s\n" name)

getCityNameIO :: IO Name
getCityNameIO = putStr "Please enter city name:" >> promptUser

getIntIO :: String -> IO (Maybe Int)
getIntIO str = putStr str >> readMaybe <$> promptUser

getListOfIntsIO :: IO [Int]
getListOfIntsIO = do
  input <- getIntIO "Please enter an integer (non-integer to finish)"
  maybe (return []) (\ i -> (i :) <$> getListOfIntsIO) input  

doFindCityIO :: [City] -> IO ()
doFindCityIO cities = do
  north <- getIntIO "Please enter city's location (degrees north):"
  east <- getIntIO "Please enter city's location (degrees east):"
  pop <- getIntIO "Please enter minimum population city should have:"
  case (north, east, pop) of
    (Just n, Just e, Just p) ->
      printf "Nearest City: %s\n\n" (nearestCityName cities (Loc n e) p)
    _ -> putStrLn $ red "Invalid population figure entered."

doPopulationIO :: [City] -> IO ()
doPopulationIO cities = do
  city <- getCityNameIO
  idx <- getIntIO "Please enter how many years ago to get records (0 for current):"
  putStrLn $ maybe (red "Please enter valid integer.") (population city) idx
  where
    population name = printf "Population: %s\n\n" . getPopulation cities name

doAnnualGrowthIO :: [City] -> IO ()
doAnnualGrowthIO cities = do
  name <- getCityNameIO
  let growths = unlines $ printf "• %.2f%%" <$> cityGrowth cities name
  if null growths
    then putStrLn "No data available.\n"
    else putStrLn $ underline "Annual Growth Figures:\n\n" ++ growths

doinsertIO :: [City] -> IO [City]
doinsertIO cities = do
  name <- getCityNameIO
  north <- getIntIO "Please enter degrees north:"
  east <- getIntIO "Please enter degrees east:"
  putStrLn "Please enter population figures (from most recent, 2+ entries)"
  records <- getListOfIntsIO
  case (north, east) of
    (Just n, Just e)
      | length records >= 2 -> return (cities `insert` City name (Loc n e) records)
      | otherwise -> do
          putStrLn $ red "Not enough population figures, not adding city.\n"
          return cities
    _ -> putStrLn (red "Invalid data, not adding city.\n") >> return cities
