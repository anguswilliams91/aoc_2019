import System.IO

fuelNeeded :: Int -> Int
fuelNeeded x = div x 3 - 2

totalFuelNeeded :: Int -> Int
totalFuelNeeded x
    | fuelNeeded x <= 0 = 0
    | otherwise = totalFuelNeeded (fuelNeeded x) + fuelNeeded x

main = do
    contents <- readFile "../inputs/dayOne.txt"
    let mass = map read (lines contents) :: [Int]   
    let partOne = sum (map fuelNeeded mass)
    let partTwo = sum (map totalFuelNeeded mass)
    print partOne
    print partTwo
