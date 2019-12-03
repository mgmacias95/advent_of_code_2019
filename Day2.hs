import System.IO
import Data.List.Split
import Data.List

restore_program :: [Int] -> [Int]
restore_program program = head program : 12 : 2 : drop 3 program

run_program :: [Int] -> Int -> [Int]
run_program program position
    | head ptail == 99 = program
    | otherwise = run_program (fhead ++ final_val:tail ftail) (position + 4)
    where
        (phead, ptail) = splitAt position program
        op = if head ptail == 1 then (+) else (*)
        val1 = program !! (head $ drop 1 ptail)
        val2 = program !! (head $ drop 2 ptail)
        fpos = head $ drop 3 ptail
        final_val = op val1 val2
        (fhead, ftail) = splitAt fpos program

main :: IO()
main = do
    contents <-  readFile "data/day2.txt"
    let program = map read $ splitOn "," contents :: [Int]
    let restored_program = restore_program program
    let final = run_program restored_program 0
    putStr $ (show $ head final) ++ "\n"
