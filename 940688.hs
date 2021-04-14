-- for finding index of element and table formatting
import Data.List     (elemIndex, intercalate)
-- for pretty printing table output
import Text.Printf   (printf)
-- for monad related function application
import Control.Monad (ap)
-- for parsing user input into Maybe a
import Text.Read     (readMaybe)

------------------------------------------------
--                   types                    --
------------------------------------------------

testData :: [City]
testData =
  [ City "Amsterdam" 52  5 [  1158,  1149,  1140,  1132],
    City "Athens"    38 23 [  3153,  3153,  3154,  3156],
    City "Berlin"    53 13 [  3567,  3562,  3557,  3552],
    City "Brussels"  51  4 [  2096,  2081,  2065,  2050],
    City "Bucharest" 44 26 [  1794,  1803,  1812,  1821],
    City "London"    52  0 [  9426,  9304,  9177,  9046],
    City "Madrid"    40  4 [  6669,  6618,  6559,  6497],
    City "Paris"     49  2 [ 11079, 11017, 10958, 10901],
    City "Rome"      42 13 [  4278,  4257,  4234,  4210],
    City "Sofia"     43 23 [  1284,  1281,  1277,  1272],
    City "Vienna"    48 16 [  1945,  1930,  1915,  1901],
    City "Warsaw"    52 21 [  1790,  1783,  1776,  1768]
  ]

data City = City
  { name        :: String,
    north       :: Int,
    east        :: Int,
    populations :: [Int]
  }
  deriving (Eq, Ord, Show, Read)

type Location = (Int, Int)

--------------------------------------------------------
--                 multi-use functions                --
--------------------------------------------------------

-- means we can do (toFloat x) rather than (fromIntegral x :: Float)
-- thanks to the type inference
toFloat :: Integral a => a -> Float
toFloat = fromIntegral

-- divide by 1000 to convert from 1000s to millions
fmtPopulation :: Integral a => a -> String
fmtPopulation = printf "%.3fm" . (/ 1000) . toFloat

location :: City -> Location
location c = (north c, east c)

nameIndex :: String -> [City] -> Maybe Int
nameIndex nm = elemIndex nm . map name

-- (Just a) if the index is valid, Nothing otherwise
maybeElemAt :: [a] -> Maybe Int -> Maybe a
_ `maybeElemAt` Nothing = Nothing
cs `maybeElemAt` (Just i)
  | i >= 0 && i < length cs = Just $ cs !! i
  | otherwise = Nothing

cityFromName :: String -> [City] -> Maybe City
cityFromName = ap maybeElemAt . nameIndex

------------------------------------------------
--                  section one               --
------------------------------------------------

-- unlines changes e.x. ["London", "Paris"] to "London\nParis\n"
getNamesString :: [City] -> String
getNamesString = unlines . getNames

getNames :: [City] -> [String]
getNames = map name

-- for user interface, output a nicer formatted list
getPrettyNamesString :: [City] -> String
getPrettyNamesString = ("\nCities:\n\n" ++) . unlines . map (("* " ++) . name)

------------------------------------------------
--                  section two               --
------------------------------------------------

getPopulation :: String -> Int -> [City] -> String
getPopulation nm yr cs = city `populationYearsAgo` yr
  where city = cityFromName nm cs

populationYearsAgo :: Maybe City -> Int -> String
Nothing `populationYearsAgo` _ = "no data"
(Just c) `populationYearsAgo` x
  | x >= 0 && x < (length . populations) c = fmtPopulation $ populations c !! x
  | otherwise = "no data"

--------------------------------------------------
--                  section three               --
--------------------------------------------------

citiesToString :: [City] -> String
citiesToString = wrap header columnLine . concatMap getCityData

getCityData :: City -> String
getCityData c = printf "| %-12s | %12d | %12d | %12s | %12s |\n"
  (name c) (north c) (east c) cur pvs
  where (cur, pvs) = getPopulationPair $ populations c

getPopulationPair :: [Int] -> (String, String)
getPopulationPair (cur : pvs : _) = (fmtPopulation cur, fmtPopulation pvs)

wrap :: [a] -> [a] -> [a] -> [a]
wrap headr footr mid = headr ++ mid ++ footr

header :: String
header = printf
    ( "%s|     Name     |  Deg. North  |   Deg. East  |"
        ++ "  Population  |   Previous   |\n%s"
    )
    columnLine
    columnLine

columnLine :: String
columnLine = wrap "+" "+\n" $ intercalate "+" (replicate 5 "--------------")

-------------------------------------------------
--                  section four               --
-------------------------------------------------

updatePopulations :: [City] -> [Int] -> [City]
updatePopulations cs pops = changed ++ unchanged
  where
    changed = zipWith addYearToRecord cs pops
    unchanged = drop (length changed) cs

addYearToRecord :: City -> Int -> City
addYearToRecord c yr = City (name c) (north c) (east c) (yr : populations c)

-------------------------------------------------
--                  section five               --
-------------------------------------------------

insertSorted :: (Ord a) => a -> [a] -> [a]
insertSorted x [] = [x]
insertSorted x xs@(next : rest)
  | x > next = next : insertSorted x rest
  | otherwise = x : xs

------------------------------------------------
--                  section six               --
------------------------------------------------

fmtGrowthFigures :: String -> [City] -> String
fmtGrowthFigures c cs = unlines [show g | g <- getGrowth c cs]

getGrowth :: String -> [City] -> [Float]
getGrowth nm = maybe [] (calcFigures . populations) . cityFromName nm

calcFigures :: [Int] -> [Float]
calcFigures [cur, pvs]       = [toFloat (cur - pvs) / toFloat pvs * 100]
calcFigures (cur : pvs : ys) = calcFigures [cur, pvs] ++ calcFigures (pvs : ys)

--------------------------------------------------
--                  section seven               --
--------------------------------------------------

nearestCityName :: Location -> Int -> [City] -> String
nearestCityName loc pop cs = maybe "no data" name (nearestCity loc pop cs)

nearestCity :: Location -> Int -> [City] -> Maybe City
nearestCity loc pop cs = idx >>= \i -> Just (cs !! i)
  where
    candidates = [c | c <- cs, head (populations c) > pop]
    dists = distancesTo loc candidates
    idx = elemIndex (minimum dists) dists

distancesTo :: Location -> [City] -> [Float]
distancesTo loc = map (distance loc . location)

distance :: Location -> Location -> Float
distance (x1, y1) (x2, y2) = sqrt . toFloat $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

------------------------------------------------------
--                  demo execution                  --
------------------------------------------------------

demo :: Int -> IO ()
demo 1 = putStr $ getNamesString testData
demo 2 = putStrLn $ getPopulation "Madrid" 2 testData
demo 3 = putStr $ citiesToString testData
demo 4 = putStr . citiesToString $ updatePopulations testData
      [1200, 3200, 3600, 2100, 1800, 9500, 6700, 11100, 4300, 1300, 2000, 1800]
demo 5 = putStr . citiesToString $ insertSorted
      (City "Prague" 50 14 [1312, 1306, 1299, 1292]) testData
demo 6 = putStr $ fmtGrowthFigures "London" testData
demo 7 = putStrLn $ nearestCityName (54, 6) 2000 testData
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

-- IO is a monad, so use mapM to apply drawCity to each city
-- just map would return [drawCity a, drawCity b, ...] (i.e. [IO ()])
drawCities :: [City] -> IO ()
drawCities cs = clearScreen >> mapM_ drawCity cs >> goTo (0, 40)

drawCity :: City -> IO ()
drawCity c = drawCityPlot (location c) (name c) $ Just c `populationYearsAgo` 0

drawCityPlot :: Location -> String -> String -> IO ()
drawCityPlot (n, e) nm pop = writeAt p1 ("+ " ++ nm) >> writeAt p2 pop
  where
    -- get abs after subtracting 54 (largest nm value in testData) to flip y
    -- axis, multiply by 2 to proportionately increase distance between cities
    y = round $ toFloat $ abs (n - 54) * 2
    x = e * 2
    p1 = (x, y)
    p2 = (x, y + 1)

------------------------------------------------------
--                  user interface                  --
------------------------------------------------------

main :: IO ()
main = do
  cs <- fileToCities "cities.txt"
  putStrLn $ getPrettyNamesString cs
  cs <- loopChoices cs
  writeFile "cities.txt" cs

fileToCities :: FilePath -> IO [City]
fileToCities = fmap (map read . lines) . readFile

showOptions :: IO ()
showOptions = putStr $ "Options:"
      ++ "\n(1) Show names"
      ++ "\n(2) Return population of city n years ago"
      ++ "\n(3) Display cities data"
      ++ "\n(4) Update data to include new population figures"
      ++ "\n(5) Add a new city to the data"
      ++ "\n(6) Get annual population growth percentage figs for a city"
      ++ "\n(7) Locate nearest city to given point above given population"
      ++ "\n(8) Draw city map"
      ++ "\n(9) Exit"
      ++ "\n> "

matchChoice :: String -> [City] -> IO ()
matchChoice choice cs
  | choice == "1" = putStrLn $ getPrettyNamesString cs
  | choice == "2" = doPopulationIO cs
  | choice == "3" = putStr $ citiesToString cs
  | choice `elem` ["4", "5", "9"] = return () -- handle these options below
  | choice == "6" = doAnnualGrowthIO cs
  | choice == "7" = doFindCityIO cs
  | choice == "8" = drawCities cs
  | otherwise = putStrLn "Invalid choice"

loopChoices :: [City] -> IO String
loopChoices cs = do
  showOptions
  choice <- getLine
  matchChoice choice cs
  -- separate case to reduce complexity of above
  cs <- case choice of
    "4" -> do
      new <- fmap (updatePopulations cs) doGetListOfInts
      putStr $ citiesToString new
      return new
    "5" -> do
      new <- doinsertSortedIO cs
      putStr $ citiesToString new
      return new
    _ -> return cs

  if choice == "9"
    -- convert cities from [City] to newline-separated String
    then return (unlines $ map show cs)
    else loopChoices cs

doCityNameIO :: IO String
doCityNameIO = putStr "Please enter city name:\n> " >> getLine

doGetInt :: IO (Maybe Int)
doGetInt = getLine >>= \x -> return (readMaybe x :: Maybe Int)

doGetListOfInts :: IO [Int]
doGetListOfInts = do
  -- any non integer input terminates the recursion
  putStr "Please enter an integer ('exit' to finish)\n> "
  input <- doGetInt
  case input of
    (Just x) -> doGetListOfInts >>= \xs -> return (x : xs)
    Nothing  -> return []

doFindCityIO :: [City] -> IO ()
doFindCityIO cs = do
  putStr "Please enter city's location (degrees north)\n> "
  north <- doGetInt
  putStr "Please enter city's location (degrees east):\n> "
  east <- doGetInt
  putStr "Please enter minimum population city should have:\n> "
  minPopulation <- doGetInt
  case (north, east, minPopulation) of
    (Just n, Just e, Just pop) ->
      putStrLn $ "\nNearest City: " ++ nearestCityName (n, e) pop cs ++ "\n"
    _ -> putStrLn "\nInvalid population figure entered.\n"

doPopulationIO :: [City] -> IO ()
doPopulationIO cs = do
  city <- doCityNameIO
  putStr "Please enter how many years ago to get records (0 for current)\n> "
  idx <- doGetInt
  case idx of
    (Just i) -> putStrLn $ "\nPopulation: " ++ getPopulation city i cs ++ "\n"
    Nothing  -> putStrLn "\nInvalid index\n"

doAnnualGrowthIO :: [City] -> IO ()
doAnnualGrowthIO cs = do
  nm <- doCityNameIO
  let growth = unlines $ printf "* %.2f%%" <$> getGrowth nm cs
  if null growth
    then putStrLn "\nNo data available.\n"
    else putStrLn $ "\nAnnual Growth Figures:\n" ++ growth

doinsertSortedIO :: [City] -> IO [City]
doinsertSortedIO cs = do
  nm <- doCityNameIO
  putStr "Please enter degrees north:\n> "
  north <- doGetInt
  putStr "Please enter degrees east:\n> "
  east <- doGetInt
  putStrLn "Please enter population figures (from most recent, 2+ entries)"
  pops <- doGetListOfInts
  case (north, east) of
    (Just n, Just e) -> return $ insertSorted (City nm n e pops) cs
    _ -> putStrLn "Invalid data given, not adding city" >> return cs
