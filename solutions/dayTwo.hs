import System.IO
import Data.Char
import Data.List.Split
import Data.Sequence hiding (splitAt, filter)
import Prelude hiding (length)

applyOp op xs i = update r (op (index xs p) (index xs q)) xs 
    where p = index xs i
          q = index xs (i + 1)
          r = index xs (i + 2)
                       
runIntCode xs i
    | index xs i == 99 = xs
    | index xs i == 1 = runIntCode ys (i + 4)
    | index xs i == 2 = runIntCode zs (i + 4)
    where ys = applyOp (+) xs (i + 1)
          zs = applyOp (*) xs (i + 1)

restoreProgram xs a b = update 2 a (update 1 b xs)

runProgram xs a b = index (runIntCode (restoreProgram xs a b) 0) 0

solveInputs xs y = 100 * (snd c) + (fst c)
    where c = head [(a, b) | a <- zs, b <- zs, (runProgram xs a b) == y]
          zs = [1..length xs - 1]

main = do
    contents <- readFile "../inputs/dayTwo.txt"
    let inputs = fromList (map read (splitOn "," contents) :: [Int])
    let partOne = runProgram inputs 2 12
    let partTwo = solveInputs inputs 19690720
    print partOne
    print partTwo



