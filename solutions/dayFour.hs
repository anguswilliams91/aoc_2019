import System.IO
import Data.Char
import Data.List

digits :: Integer -> [Integer]
digits n = [toInteger (digitToInt x) | x <- show n]

neverDecrease :: Integer -> Bool
neverDecrease n = and $ map (\x -> fst x >= snd x) xs
    where xs = zip (tail ys) (init ys)
          ys = digits n 

hasDouble :: Integer -> Bool
hasDouble n = or $ map (\x -> fst x == snd x) xs
    where xs = zip (tail ys) (init ys)
          ys = digits n

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)
          
atLeastOneDouble :: Integer -> Bool
atLeastOneDouble n = elem 2 [count x xs | x <- nub xs]
    where xs = digits n

partOne :: Integer -> Integer -> [Integer]
partOne x y = filter (\n -> neverDecrease n && hasDouble n) [x..y]

partTwo :: Integer -> Integer -> [Integer]
partTwo x y = filter (\n -> neverDecrease n && hasDouble n && atLeastOneDouble n) [x..y]

main = do
    print . length $ partOne 153517 630395
    print . length $ partTwo 153517 630395