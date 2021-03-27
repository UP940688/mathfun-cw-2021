--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--


--
-- Types (define City type here)
--

testData :: [City]
testData = [ ... the 12 City values ... ]


--
--  Your functional code goes here
--


--
--  Demo
--

demo :: Int -> IO ()
demo 1 = -- output the names of all the cities
demo 2 = -- output the population of "Madrid" 2 years ago
demo 3 = putStrLn (citiesToString testData)
demo 4 = -- output the data (as for (iii)) after it has been updated with the
         -- following new population figures (the first is for Amsterdam, etc.)
         -- [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800]
demo 5 = -- show the data (as for (iii)) after adding "Prague" (50N, 14E) 
         -- with population figures [1312, 1306, 1299, 1292]
demo 6 = -- output a list of annual growth figures for "London"
demo 7 = -- output the nearest city to location (54N ,6E) with 
         -- a population above 2m people
demo 8 = -- output the population map


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
 
