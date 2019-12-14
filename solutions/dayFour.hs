import System.IO
import Data.Char

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


validPasswords :: Integer -> Integer -> [Integer]
validPasswords x y = filter (\n -> neverDecrease n && hasDouble n) [x..y]

main = do
    print . length $ validPasswords 153517 630395