-- For finding index of elements and table formatting
import Data.List (elemIndex, intercalate)
-- For filtering [Maybe a] -> [a]
import Data.Maybe (catMaybes)
-- For string formatting
import Text.Printf (printf)
-- For parsing IO String -> IO (Maybe a)
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

-- | The City algebraic type should have a Name, Location, and
-- [Population] list that contains at least two years of data.
data City = City
  { getName :: Name,
    getLocation :: Location,
    getRecords :: [Population]
  }
  deriving (Eq, Ord, Show, Read)

-- | Comprised of one Int representing Degrees North, one Degrees East.
data Location = Loc Int Int deriving (Eq, Ord, Show, Read)

-- | Int representing population in 1000s.
type Population = Int

-- | Int representing an index to a list.
type Index = Int

-- | Distance between two Locations calculated via distance formula.
type Distance = Float

-- | Float representing the percentage growth between two Population types.
type Growth = Float

-- | String form of Population formatted as X.YZm (or 'no data').
type FormattedPopulation = String

-- | String representing the name of a city.
type Name = String

-- | String that is intended to be displayed to Standard Output.
type OutString = String

--------------------------------------------------------
--                  helper functions                  --
--------------------------------------------------------

-- | Converts an integral (Int/Integer) to a Float.
toFloat :: Integral a => a -> Float
toFloat = fromIntegral

-- | Returns (Just City) if given valid name, Nothing otherwise.
cityFromName :: [City] -> Name -> Maybe City
cityFromName cities name = Just (cities !!) <*> elemIndex name (getNames cities)

-- | Divides a Population by 1000 and converts to FormattedPopulation.
fmtPopulation :: Population -> FormattedPopulation
fmtPopulation = printf "%.3fm" . (/ 1000) . toFloat

------------------------------------------------
--                  section one               --
------------------------------------------------

-- | Partial function that will map getName over a list of cities.
getNames :: [City] -> [Name]
getNames = map getName

------------------------------------------------
--                  section two               --
------------------------------------------------

-- | Composes the populationAt and cityFromName functions.
getPopulation :: [City] -> Name -> Index -> FormattedPopulation
getPopulation = fmap populationAt . cityFromName

-- | Returns Population for given city in given year, if the index is valid.
maybeRecord :: Index -> City -> Maybe Population
maybeRecord i (City _ _ records)
  | i >= 0 && i < length records = Just (records !! i)
  | otherwise = Nothing

-- | Returns FormattedPopulation of (Maybe City) from Index years ago.
populationAt :: Maybe City -> Index -> FormattedPopulation
populationAt city idx = maybe "no data" fmtPopulation (maybeRecord idx =<< city)

--------------------------------------------------
--                  section three               --
--------------------------------------------------

-- | Partial function that composes wrap with concatenated map of cityRow over cities
citiesToString :: [City] -> OutString
citiesToString = wrap . concatMap cityRow

-- | Returns a well formatted row of data representing given city
cityRow :: City -> OutString
cityRow (City name (Loc n e) records) =
  printf "| %-12s | %12d | %12d | %12s | %12s |\n" name n e cur pvs
  -- take advantage of haskell's laziness -- it will only map 2 elements
  where [cur, pvs] = (take 2 . map fmtPopulation) records

-- | Wrap a string with the table header and a footer
wrap :: String -> OutString
wrap text = (line ++ header ++ line) ++ text ++ line

-- | Table header intended for use with wrap
header :: OutString
header = "|     Name     |  Deg. North  |   Deg. East  |  Population  |   Previous   |\n"

-- | Line of '-' and '+' chars intended for use with wrap
line :: OutString
line = '+' : intercalate "+" (replicate 5 "--------------") ++ "+\n"

-------------------------------------------------
--                  section four               --
-------------------------------------------------

-- | Given a list of cities and list of population records, return
-- an updated list of cities reflecting the new data.
updateRecords :: [City] -> [Population] -> [City]
cities `updateRecords` pops = changed ++ unchanged
  where
    changed = zipWith addYear cities pops
    unchanged = drop (length changed) cities

-- | Return new City, shifting population records back one year and prepending one
addYear :: City -> Population -> City
addYear (City name loc records) pop = City name loc (pop : records)

-------------------------------------------------
--                  section five               --
-------------------------------------------------

-- | Insert an element into a sorted list (must implement Ord)
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
  printf "\nINFO: Loaded %s cities.\n\n" $ colour (printf "%i/%i" len2 len1)
  putStrLn (getPrettyNamesString cities)
  updatedCities <- loopChoices cities
  writeFile "cities.txt" updatedCities

-- fmap to map over IO Monad, then map (readMaybe String :: Maybe City)
fileToCities :: FilePath -> IO [Maybe City]
fileToCities fp = map readMaybe . lines <$> readFile fp

showOptions :: IO ()
showOptions = putStr $ underline "\nOptions:" ++
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
      putStr $ citiesToString new
      return new
    "5" -> do
      new <- doinsertIO cities
      putStr $ citiesToString new
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
  | otherwise = putStrLn (red "Invalid choice.")

getPrettyNamesString :: [City] -> String
getPrettyNamesString = (underline "City Names:\n" ++) . nameList
  where nameList = concatMap (\ (City name _ _) -> printf "\n• %s" name)

getCityNameIO :: IO Name
getCityNameIO = putStr "Please enter city name:" >> promptUser

getIntIO :: String -> IO (Maybe Int)
getIntIO str = putStr str >> readMaybe <$> promptUser

getLocationIO :: String -> IO (Maybe (Int, Int))
getLocationIO str = putStr str >> readMaybe <$> promptUser

getListOfIntsIO :: IO [Int]
getListOfIntsIO = do
  input <- getIntIO "Please enter an integer (non-integer to finish)"
  maybe (return []) (\ i -> (i :) <$> getListOfIntsIO) input

doFindCityIO :: [City] -> IO ()
doFindCityIO cities = do
  loc <- getLocationIO "Please enter city's location in form (N, E):"
  pop <- getIntIO "Please enter minimum population city should have:"
  putStrLn $ case (loc, pop) of
    (Just (n, e), Just p) -> "Nearest City: " ++ nearestCityName cities (Loc n e) p
    _ -> red "Invalid population figure entered."

doPopulationIO :: [City] -> IO ()
doPopulationIO cities = do
  name <- getCityNameIO
  idx <- getIntIO "Please enter how many years ago to get records (0 = current):"
  putStrLn $ maybe (red "Please enter valid integer.") (populationOf name) idx
  where
    populationOf name = printf "Population: %s" . getPopulation cities name

doAnnualGrowthIO :: [City] -> IO ()
doAnnualGrowthIO cities = do
  name <- getCityNameIO
  let growths = concat $ printf "\n• %.2f%%" <$> cityGrowth cities name
  putStrLn $ if null growths
    then "No data available."
    else underline "Annual Growth Figures:\n" ++ growths

doinsertIO :: [City] -> IO [City]
doinsertIO cities = do
  name <- getCityNameIO
  maybeLoc <- getLocationIO "Please enter city's location in form (N, E):"
  putStrLn "Please enter population figures (from most recent, 2+ entries)"
  pops <- getListOfIntsIO
  maybe (err "Invalid data, not adding city." cities) (\ (n, e) ->
      if length pops >= 2
        then return (cities `insert` City name (Loc n e) pops)
        else err "Not enough population figures, not adding city." cities
    ) maybeLoc
  where err str c = putStrLn (red str) >> return c
