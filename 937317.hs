-- UP937317
--TO DO list:
    -- sanitaze user input
    -- UI overwriting map

import Text.Printf (printf)
import Data.List (sortBy)
import Data.Char

data City = City {
    name :: String,
    degNorth :: Int,
    degEast :: Int ,
    populationYears :: [Int]
} deriving (Show, Read, Eq)

testData :: [City]
testData = [
    City "Amsterdam"    52   5    [1158, 1149, 1140, 1132],
    City "Athens"       38  23    [3153, 3153, 3154, 3156],
    City "Berlin"       53  13    [3567, 3562, 3557, 3552],
    City "Brussels"     51   4    [2096, 2081, 2065, 2050],
    City "Bucharest"    44  26    [1794, 1803, 1812, 1821],
    City "London"       52   0    [9426, 9304, 9177, 9046],
    City "Madrid"       40   4    [6669, 6618, 6559, 6497],
    City "Paris"        49   2    [11079, 11017, 10958, 10901],
    City "Rome"         42  13    [4278, 4257, 4234, 4210],
    City "Sofia"        43  23    [1284, 1281, 1277, 1272],
    City "Vienna"       48  16    [1945, 1930, 1915, 1901],
    City "Warsaw"       52  21    [1790, 1783, 1776, 1768]
        ]


-- Demo 2

-- outputs the int of the population to next function

population :: String -> Int -> [City]-> Int
population cN x cities = populationYears (location cN cities) !! x 
                where-- search for the city
                    location outputName (x:xs) 
                                      | outputName == name x = x 
                                      | otherwise = location outputName xs

-- formating the output correctly

formatPopulation :: String -> Int -> [City]-> String
formatPopulation cN x cities
        | x <= 3 && x >= 0 = show ( fromIntegral (population cN x cities) / 1000) ++ "m"
        | otherwise = "No data"

-- Demo 3

--Outputs string of all values + \n and \t for formatting

formatCities :: [City] -> String
formatCities [] = []
formatCities (x:xs)
    | length (name x) < 7 = name x ++ "\t\t" ++ show (degNorth x) ++ "\t" ++ show (degEast x) ++ "\t" ++ formatPopulation (name x) 0 (x:xs) ++ "\t\t" ++ formatPopulation (name x) 1 (x:xs) ++ "\n" ++ formatCities xs
    | otherwise = name x ++ "\t" ++ show (degNorth x) ++ "\t" ++ show (degEast x) ++ "\t" ++ formatPopulation (name x) 0 (x:xs) ++ "\t\t" ++ formatPopulation (name x) 1 (x:xs) ++"\n" ++ formatCities xs

-- Demo 4

-- inserts the created list from addYears to the lis tof Cities

updatePopulation :: [City] -> [Int] -> [City]
updatePopulation [] [] = []
updatePopulation (x:xs) (y:ys) = City { name = name x, degNorth = degNorth x, degEast = degEast x, populationYears = addYears (x:xs) (y:ys)} : updatePopulation xs ys

-- outputs list of previous population + new one

addYears :: [City] -> [Int] -> [Int]
addYears (x:xs) (y:ys) = y : populationYears x

-- Demo 5

--crates new city and populates it

newCity :: String -> Int -> Int -> [Int] -> City
newCity newName newdegNorth newdegEast newpopulationYears = City { name = newName, degNorth = newdegNorth, degEast = newdegEast, populationYears = newpopulationYears }

-- ads the city to the list in its correct oreder

allCities :: City -> [City] -> [City]
allCities city [] = []
allCities city (x:xs)
    | name city > name x = x:allCities city xs
    | otherwise = city: x:xs

-- Demo 6

-- recursively finds name of city

findCity :: String -> [City] -> City
findCity cN (x:xs) | cN == name x = x
                           | otherwise = findCity cN xs

-- rounds to 2 decimal

roundto2Decimal :: Double -> Double
roundto2Decimal x = fromIntegral (round $ x * 1e4) / 1e4

-- Outputs the population growth

populationGrowth :: City -> [Double]
populationGrowth x =  map (* 100) [roundto2Decimal((fromIntegral (head(populationYears x)) - fromIntegral (populationYears x !! 1)) / fromIntegral (head(populationYears x))),
                                         roundto2Decimal((fromIntegral (populationYears x !! 1) - fromIntegral (populationYears x !! 2)) / fromIntegral (populationYears x !! 1)),
                                         roundto2Decimal((fromIntegral (populationYears x !! 2) - fromIntegral (populationYears x !! 3)) / fromIntegral (populationYears x !! 2))]

-- Demo 7

closestCity :: Int  -> Int -> Int -> [City] -> String
closestCity _ _ _ []= "No City"
closestCity dN dE pop (x:xs)
        | distSort (howFar (populationCheck pop testData )) (distance dN dE) == distance (degNorth x) (degEast x) = name x
        | otherwise = closestCity dN dE pop xs
        where
            distance a b = sqrt(fromIntegral a^2+fromIntegral b^2)

--sorting by distance

distSort :: [Double] -> Double -> Double
distSort list distance = sortBy (\n1 n2 -> abs (n1 - distance) `compare` abs (n2 - distance)) list !!0

--calculates the distance (the forula)

howFar :: [City] -> [Double]
howFar [] = []
howFar xs= map (\ x-> sqrt(fromIntegral (degNorth x ^ 2)+ fromIntegral (degEast x ^ 2)))xs

--recursively compares if the city has minimum population specified

populationCheck :: Int -> [City] -> [City]
populationCheck _ [] = []
populationCheck minimumPop (x:xs)
        | minimumPop < head (populationYears x) = x: populationCheck minimumPop xs
        | otherwise = populationCheck minimumPop xs

-- 
--  Demo code
--

demo :: Int -> IO ()

-- putStrLn for \n and \t

demo 1 =  print [name listOfNames | listOfNames <- testData]

demo 2 = print (formatPopulation "Madrid" 2 testData)
 
demo 3 = putStrLn (formatCities testData)
-- output the data (as for (iii)) after it has been updated with the
 -- following new population figures (the first is for Amsterdam, etc.)
  -- [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800]

demo 4 = putStrLn (formatCities (updatePopulation testData [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800]) )

-- demo 5 =-- show the data (as for (iii)) after adding "Prague" (50N, 14E) 
         -- with population figures [1312, 1306, 1299, 1292]

demo 5 = putStrLn (formatCities (allCities (newCity "Prague" 50 14 [1312, 1306, 1299, 1292]) testData ))

-- demo 6 = -- Outputs population growth for London

demo 6 = print [populationGrowth (findCity "London" testData)]

-- demo 7 =  -- output the nearest city to location (54N ,6E) with 
             -- a population above 2m people

demo 7 = print (closestCity 54 6 2 testData)

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

-- format the string correctyl
format :: (String, Int, Int, Int) -> String
format (name, dN, dE, pop) =  show(dN) ++ "N" ++ " " ++ show(dE) ++ "E" ++  " + " ++ name ++ " + " ++ show(pop)

--convert Record City to tuple for better handling
fromRecordToTuple :: City -> (String, Int, Int, Int)
fromRecordToTuple c = (name c, degNorth c, degEast c, populationYears c !! 0)

--output coordinates of city to tuple
fromRecordPosition :: City -> (Int, Int)
fromRecordPosition c = (degNorth c, degEast c)


-- inetrate through cities and build map
populationMap :: [City] -> IO()
populationMap (x:xs) = do 
        if length xs > 0 then do
                writeAt (fromRecordPosition x) (format(fromRecordToTuple x))
                populationMap xs
        else return()

--clear screen before building map (main function for Map)
mapMain :: [City] -> IO()
mapMain (x:xs) = do
    clearScreen
    populationMap (x:xs)
--
-- Your user interface (and loading/saving) code goes here
--
 

--loadCities
loadFile :: IO [City]
loadFile = do
    file <- readFile "./cities.txt" -- content
    return (read file :: [City]) 

main :: IO ()
main = do
    citiesFromFile <- loadFile
    putStrLn "Cities: "
    print [name listOfNames | listOfNames <- citiesFromFile]
    ui citiesFromFile

ui :: [City] -> IO ()
ui citiesList = do
    putStrLn "#######################################################################################"
    putStrLn "1. Output names of all Cities in the list."
    putStrLn "2. Output the population for City, x years ago."
    putStrLn "3. Output the current list of cities."
    putStrLn "4. Update population values for cities (demo 4)."
    putStrLn "5. Add a new city to the list with predefined population values (from demo 5)."
    putStrLn "6. Output annual growth in percentage for a city."
    putStrLn "7. Output nearest city to your location."
    putStrLn "8. Output map of cities with some of the values."
    putStrLn "0. Save and exit."
    putStrLn "#######################################################################################\n"
    putStrLn "Enter the number of option you want to select:"
    option <- getLine
    -- sanitize input
    backend option citiesList

backend :: String -> [City] -> IO()
backend "0" listofCities = do
    putStrLn("#######################################################################################")
    putStrLn("Saving changes into file cities.txt\n")
    length listofCities `seq` writeFile "cities.txt" (show listofCities)
    putStrLn("Exiting program.\n")


backend "1" listofCities = do
    putStrLn("#######################################################################################")
    putStrLn("Names of all cities in current list are: ")
    print [name listOfNames | listOfNames <- listofCities]
    ui listofCities

backend "2" listofCities = do
    putStrLn("#######################################################################################")
    putStrLn "Enter name of the city: "
    enteredCity <- getLine
    let showCity = justLetters (firstLetterBig enteredCity)
    -- sanitize input

    putStrLn "Enter how many years ago you want to view its population: "
    enteredYear <- getLine
    let year = read enteredYear :: Int
    -- sanitize input

    putStrLn("#######################################################################################")
    print (formatPopulation showCity year listofCities)
    ui listofCities

backend "3" listofCities = do
    putStrLn("#######################################################################################")
    putStrLn (formatCities listofCities)
    ui listofCities

backend "4" listofCities = do
    putStrLn("#######################################################################################")
    -- just demo 4 
    putStrLn (formatCities (updatePopulation testData [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800]) )
    ui listofCities

backend "5" listofCities = do
    --ADDING POPULATION VALUES ????

    putStrLn("#######################################################################################")
    putStrLn "Enter name of the city: "
    enteredCity <- getLine
    let showCity = justLetters (firstLetterBig enteredCity)
    -- sanitize input   

    putStrLn "Enter coordinates of the city, degrees north: "
    entereddegNorth <- getLine
    let degN = read entereddegNorth :: Int
    -- sanitize input

    putStrLn "Enter coordinates of the city, degrees easy: "
    entereddegEast <- getLine
    let degE = read entereddegEast :: Int
    -- sanitize input

    putStrLn("#######################################################################################")
    let newCities = allCities (newCity showCity degN degE [1312, 1306, 1299, 1292]) listofCities
    putStrLn (formatCities (allCities (newCity showCity degN degE [1312, 1306, 1299, 1292]) listofCities ))
    ui newCities


backend "6" listofCities = do
    putStrLn("#######################################################################################")
    putStrLn "Enter name of the city: "
    enteredCity <- getLine
    let showCity = justLetters (firstLetterBig enteredCity)
    -- sanitize input  

    putStrLn("#######################################################################################")
    print [populationGrowth (findCity showCity listofCities)]
    ui listofCities

backend "7" listofCities = do
    putStrLn("#######################################################################################")
    putStrLn "Enter coordinates of location, degrees north: "
    entereddegNorth <- getLine
    let degN = read entereddegNorth :: Int
    -- sanitize input

    putStrLn "Enter coordinates of location, degrees east: "
    entereddegEast <- getLine
    let degE = read entereddegEast :: Int
    -- sanitize input

    putStrLn("Enter number of millions as minimal value for population: (in millions)")
    entereddegPopulation <- getLine
    let pop = read entereddegPopulation :: Int
    -- sanitize input
    
    putStrLn("#######################################################################################")
    print (closestCity degN degE pop listofCities)
    ui listofCities

backend "8" listofCities = do
    mapMain listofCities
    ui listofCities

---------------HELPER FUNCTIONS FOR INPUT SANITIZING--------------------- 

--Capitalize first letter of entered city (if not already)
firstLetterBig :: String -> String
firstLetterBig (x:xs) = toUpper x : map toLower xs
firstLetterBig [] = []

--removes non letter characters
justLetters :: String -> String
justLetters = filter isLetter