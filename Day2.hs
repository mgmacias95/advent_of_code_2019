import System.IO
import Data.List.Split
import Data.List

replace_noun_verb :: [Int] -> (Int, Int) -> [Int]
replace_noun_verb program (noun, verb) = head program : noun : verb : drop 3 program

run_program :: Int -> [Int] -> [Int]
run_program position program
    | head ptail == 99 = program
    | otherwise = run_program (position + 4) (fhead ++ final_val:tail ftail)
    where
        (phead, ptail) = splitAt position program
        op = if head ptail == 1 then (+) else (*)
        val1 = program !! (head $ drop 1 ptail)
        val2 = program !! (head $ drop 2 ptail)
        fpos = head $ drop 3 ptail
        final_val = op val1 val2
        (fhead, ftail) = splitAt fpos program

find_combination :: [Int] -> (Int, Int)
find_combination program = head $ dropWhile (\(x, y) -> head (run_program 0 $ replace_noun_verb program (x, y)) /= 19690720) (concat $ map (\y -> map (\x -> (y, x)) [0..99]) [0..99])

main :: IO()
main = do
    contents <-  readFile "data/day2.txt"
    let program = map read $ splitOn "," contents :: [Int]
    let (noun, verb) = find_combination program
    putStr $ show (100 * noun + verb) ++ "\n"
