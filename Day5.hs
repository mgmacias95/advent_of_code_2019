import System.IO
import System.IO.Unsafe
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

get_input :: IO Int
get_input = do
    input <- getLine
    let io_int_input = read input :: Int
    return io_int_input


run_program :: Int -> [Int] -> [Int]
run_program position program = case (d*10 + e) of 99 -> program
                                                  1  -> run_program (position + 4) (update_list program (v1 + v2) i3)
                                                  2  -> run_program (position + 4) (update_list program (v1 * v2) i3)
                                                  3  -> run_program (position + 2) (update_list program (unsafePerformIO get_input) i1)
                                                  4  -> run_program (position + 2) (program ++ [v1])
    where
        (_, ptail) = splitAt position program
        a:b:c:d:e:[] = zfill 5 $ list_digits $ head ptail
        _:i1:[] = take 2 ptail
        _:_:i2:i3:[] = take 4 ptail
        v1 = if c == 0 then (program !! i1) else i1
        v2 = if b == 0 then (program !! i2) else i2


main :: IO()
main = do
    contents <-  readFile "data/day5.txt"
    let program = map read $ splitOn "," contents :: [Int]
    let result = run_program 0 program
    putStr $ (show $ last result) ++ "\n"
