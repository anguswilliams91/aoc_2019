import System.IO
import Data.Char
import Data.List.Split
import Data.Sequence hiding (splitAt)

applyOp op xs i = update r (op (index xs p) (index xs q)) xs where p = index xs i
                                                                   q = index xs (i + 1)
                                                                   r = index xs (i + 2)
                                                                   
runIntCode xs i
    | index xs i == 99 = xs
    | index xs i == 1 = runIntCode ys (i + 4)
    | index xs i == 2 = runIntCode zs (i + 4)
    where ys = applyOp (+) xs (i + 1)
          zs = applyOp (*) xs (i + 1)

restoreProgram xs = update 2 2 (update 1 12 xs)

main = do
    contents <- readFile "../inputs/dayTwo.txt"
    let inputs = fromList (map read (splitOn "," contents) :: [Int])
    let partOne = index (runIntCode (restoreProgram inputs) 0) 0
    print partOne

