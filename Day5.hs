import System.IO
import Data.List.Split
import Data.List
import Data.Char

list_digits :: Int -> [Int]
list_digits = map (fromIntegral . digitToInt) . show

zfill :: Int -> [Int] -> [Int]
zfill n lst = (take (n - length lst) $ repeat 0) ++ lst

update_list :: [a] -> a -> Int -> [a]
update_list lst elem pos = lhead ++ elem:tail ltail
    where
        (lhead, ltail) = splitAt pos lst


run_program :: [Int] -> Int -> [Int] -> [Int]
run_program input position program = case (d*10 + e) of 99 -> program
                                                        1  -> run_program input (position + 4) (update_list program (v1 + v2) i3)
                                                        2  -> run_program input (position + 4) (update_list program (v1 * v2) i3)
                                                        3  -> run_program (tail input) (position + 2) (update_list program (head input) i1)
                                                        4  -> run_program input (position + 2) (program ++ [v1])
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


main :: IO()
main = do
    contents <- readFile "data/day5.txt"
    let program = map read $ splitOn "," contents :: [Int]
    input <- getLine
    let int_input = read input :: Int
    let result = run_program [int_input] 0 program
    putStr $ (show $ last result) ++ "\n"
