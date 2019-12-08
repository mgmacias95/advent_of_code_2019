import Day5
import Data.List
import Data.List.Split

run_single_comb :: Int -> [Int] -> [Int] -> Int
run_single_comb i0 program settings = foldl (\x y -> last $ run_program [y, x] 0 program) i0 settings

run_all_combs :: (Int, Int) -> Int -> [Int] -> Int
run_all_combs (start, end) i0 program = maximum $ map (run_single_comb i0 program) $ permutations [start..end]

main :: IO()
main = do
    contents <- readFile "data/day7.txt"
    let program = map read $ splitOn "," contents :: [Int]
    let result = run_all_combs (0,4) 0 program
    putStr $ (show result) ++ "\n"
