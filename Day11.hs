import System.IO
import Data.List.Split
import Data.List
import Data.Char
import qualified Data.Map.Lazy as Map

list_digits :: Int -> [Int]
list_digits = map (fromIntegral . digitToInt) . show

zfill :: Int -> [Int] -> [Int]
zfill n lst = (take (n - length lst) $ repeat 0) ++ lst

update_list :: (Num a) => [a] -> a -> Int -> [a]
update_list lst elem pos
    | pos < l  = lhead ++ elem:tail ltail
    | otherwise = lst ++ (replicate (pos - l) 0) ++ [elem]
    where
        (lhead, ltail) = splitAt pos lst
        l = length lst

parameter_mode :: Int -> Int -> Int -> [Int] -> Int
parameter_mode mode i rbase program = case mode of 0 -> if (i < length(program)) then program !! i else 0
                                                   1 -> i
                                                   2 -> if (new_rbase < length(program)) then program !! new_rbase else 0
    where
        new_rbase = i + rbase

class (Enum a, Bounded a, Eq a) => Circ a where
    next :: a -> a
    next a = if a == maxBound then minBound else succ a

    prev :: a -> a
    prev a = if a == minBound then maxBound else pred a

data Direction = U | L | D | R deriving (Ord, Show, Enum, Bounded, Eq)

instance Circ Direction

new_input :: [Int] -> Map.Map (Int,Int) Int -> (Int,Int) -> Direction -> ([Int], Map.Map (Int,Int) Int, (Int,Int), Direction)
new_input [x] robotpos robotcur robotdir = ([], robotpos, robotcur, robotdir)
new_input (x1:x2:[]) robotpos (cx,cy) robotdir = ([nextcol], nextpos, nextcur, nextdir)
    where
        nextdir = if (x2 == 1) then prev robotdir else next robotdir
        nextcur = (cx + (if (nextdir == R) then -1 else (if (nextdir == L) then 1 else 0)), cy + (if (nextdir == U) then -1 else (if (nextdir == D) then 1 else 0)))
        nextpos = Map.insert (cx,cy) x1 robotpos
        nextcol = Map.findWithDefault 0 nextcur nextpos

run_program :: [Int] -> Int -> Int -> [Int] -> Map.Map (Int,Int) Int -> (Int,Int) -> Direction -> [Int] -> Map.Map (Int,Int) Int
run_program input position rbase output robotpos robotcur robotdir program = case (d*10 + e) of 99 -> robotpos
                                                                                                1  -> run_program input (position + 4) rbase output robotpos robotcur robotdir (update_list program (v1 + v2) v3)
                                                                                                2  -> run_program input (position + 4) rbase output robotpos robotcur robotdir (update_list program (v1 * v2) v3)
                                                                                                3  -> run_program (tail input) (position + 2) rbase output robotpos robotcur robotdir (update_list program (head input) ii1)
                                                                                                4  -> run_program next_i (position + 2) rbase next_o nextpos nextcur nextdir program
                                                                                                5  -> run_program input (if v1 == 0 then (position + 3) else v2) rbase output robotpos robotcur robotdir program
                                                                                                6  -> run_program input (if v1 /= 0 then (position + 3) else v2) rbase output robotpos robotcur robotdir program
                                                                                                7  -> run_program input (position + 4) rbase output robotpos robotcur robotdir (update_list program (if v1 < v2 then 1 else 0) v3)
                                                                                                8  -> run_program input (position + 4) rbase output robotpos robotcur robotdir (update_list program (if v1 == v2 then 1 else 0) v3)
                                                                                                9  -> run_program input (position + 2) (v1 + rbase) output robotpos robotcur robotdir program
    where
        (_, ptail) = splitAt position program
        a:b:c:d:e:[] = zfill 5 $ list_digits $ head ptail
        i0:i1:[] = take 2 ptail
        _:_:i2:i3:[] = take 4 ptail
        v1 = parameter_mode c i1 rbase program
        v2 = parameter_mode b i2 rbase program
        v3 = if (a == 2) then (i3 + rbase) else i3
        ii1 = if (c == 2) then (i1 + rbase) else i1
        next_o = if (length output == 2) then [v1] else output ++ [v1]
        (next_i, nextpos, nextcur, nextdir) = new_input next_o robotpos robotcur robotdir

main :: IO()
main = do
    contents <- readFile "data/day11.txt"
    let program = map read $ splitOn "," contents :: [Int]
    let result = run_program [0] 0 0 [] (Map.fromList []) (0,0) U program
    putStr $ (show $ length result) ++ "\n"
