import System.IO
import Data.List.Split
import Data.List
import Data.Char

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

run_program :: [Int] -> Int -> Int -> [Int] -> [Int] -> [Int]
run_program input position rbase output program = case (d*10 + e) of 99 -> output
                                                                     1  -> run_program input (position + 4) rbase output (update_list program (v1 + v2) v3)
                                                                     2  -> run_program input (position + 4) rbase output (update_list program (v1 * v2) v3)
                                                                     3  -> run_program (tail input) (position + 2) rbase output (update_list program (head input) ii1)
                                                                     4  -> run_program input (position + 2) rbase (output ++ [v1]) program
                                                                     5  -> run_program input (if v1 == 0 then (position + 3) else v2) rbase output program
                                                                     6  -> run_program input (if v1 /= 0 then (position + 3) else v2) rbase output program
                                                                     7  -> run_program input (position + 4) rbase output (update_list program (if v1 < v2 then 1 else 0) v3)
                                                                     8  -> run_program input (position + 4) rbase output (update_list program (if v1 == v2 then 1 else 0) v3)
                                                                     9  -> run_program input (position + 2) (v1 + rbase) output program
    where
        (_, ptail) = splitAt position program
        a:b:c:d:e:[] = zfill 5 $ list_digits $ head ptail
        i0:i1:[] = take 2 ptail
        _:_:i2:i3:[] = take 4 ptail
        v1 = parameter_mode c i1 rbase program
        v2 = parameter_mode b i2 rbase program
        v3 = if (a == 2) then (i3 + rbase) else i3
        ii1 = if (c == 2) then (i1 + rbase) else i1

process_game :: [Int] -> Int
process_game program = length $ filter (\x -> last x == 2) chunks
    where
        chunks = chunksOf 3 $ run_program [] 0 0 [] program

main :: IO()
main = do
    contents <- readFile "data/day13.txt"
    let program = map read $ splitOn "," contents :: [Int]
    let result = process_game program
    putStr $ (show result) ++ "\n"
