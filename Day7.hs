import System.IO
import Data.List
import Data.List.Split
import Data.Char

list_digits :: Int -> [Int]
list_digits = map (fromIntegral . digitToInt) . show

zfill :: Int -> [Int] -> [Int]
zfill n lst = (take (n - length lst) $ repeat 0) ++ lst

update_list :: [a] -> a -> Int -> [a]
update_list lst elem pos = lhead ++ elem:tail ltail
    where
        (lhead, ltail) = splitAt pos lst


run_program :: [Int] -> Int -> [Int] -> (Int, [Int])
run_program input position program = case (d*10 + e) of 99 -> (-1, program)
                                                        1  -> run_program input (position + 4) (update_list program (v1 + v2) i3)
                                                        2  -> run_program input (position + 4) (update_list program (v1 * v2) i3)
                                                        3  -> run_program (tail input) (position + 2) (update_list program (head input) i1)
                                                        4  -> ((position + 2), program ++ [v1])
                                                        5  -> run_program input (if v1 == 0 then (position + 3) else v2) program
                                                        6  -> run_program input (if v1 /= 0 then (position + 3) else v2) program
                                                        7  -> run_program input (position + 4) (update_list program (if v1 < v2 then 1 else 0) i3)
                                                        8  -> run_program input (position + 4) (update_list program (if v1 == v2 then 1 else 0) i3)
    where
        (_, ptail) = splitAt position program
        a:b:c:d:e:[] = zfill 5 $ list_digits $ head ptail
        i0:i1:[] = take 2 ptail
        _:_:i2:i3:[] = take 4 ptail
        v1 = if c == 0 then (program !! i1) else i1
        v2 = if b == 0 then (program !! i2) else i2


run_single_comb :: [[Int]] -> [(Int, [Int])] -> [(Int, [Int])]
run_single_comb _ [] = []
run_single_comb [i] ((pos, prog):xs) = run_program i pos prog : run_single_comb [] []
run_single_comb (i:n_i:input) ((pos, prog):xs) = iter_out : run_single_comb ((n_i ++ [last $ snd iter_out]):input) xs
    where
        iter_out = run_program i pos prog


run_all_combs :: [[Int]] -> [(Int, [Int])] -> [(Int, [Int])]
run_all_combs i0 input
    | (fst $ last input) == -1 = input
    | otherwise                = run_all_combs ([output_from_last_iter]:(replicate 4 [])) output
    where
        output = run_single_comb i0 input
        output_from_last_iter = last $ snd $ last output

interface :: Int -> [Int] -> [Int] -> Int
interface i0 program (s:settings) = last $ snd $ last $ run_all_combs ([s, i0]:(map (\x -> [x]) settings)) (replicate 5 (0, program))

get_max :: (Int,Int) -> Int -> [Int] -> Int
get_max (start, end) i0 program = maximum $ map (interface i0 program) $ permutations [start..end]

main :: IO()
main = do
    contents <- readFile "data/day7.txt"
    let program = map read $ splitOn "," contents :: [Int]
    let result = get_max (5,9) 0 program
    putStr $ (show result) ++ "\n"
