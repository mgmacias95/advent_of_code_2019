import System.IO
import Data.List.Split
import Data.List
import Data.String


sum_tuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
sum_tuples (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)


move_coordinates :: [[Char]] -> (Int, Int) -> [(Int, Int)]
move_coordinates [] (_, _) = []
move_coordinates ((dir:steps):moves) (c1, c2)
    | dir == 'R' = generate_points (0,1) ++ move_coordinates moves (c1, c2 + nsteps)
    | dir == 'U' = generate_points (1,0) ++ move_coordinates moves (c1 + nsteps, c2)
    | dir == 'L' = generate_points (0,-1) ++ move_coordinates moves (c1, c2 - nsteps)
    | dir == 'D' = generate_points (-1,0) ++ move_coordinates moves (c1 - nsteps, c2)
    | otherwise  = error "Invalid move"
    where
        nsteps = read steps :: Int
        generate_points x = take nsteps $ iterate (sum_tuples x) (c1, c2)


max_manhattan :: (Int, Int) -> [(Int, Int)] -> Int
max_manhattan (i1, i2) coords = minimum $ map (\(x, y) -> abs (x - i1) +  abs (y - i2)) coords


main :: IO()
main = do
    contents <-  readFile "data/day3.txt"
    let wire1:wire2:[] = map (splitOn ",") $ lines contents
    let max_int = max_manhattan (0,0) $ tail $ (move_coordinates wire1 (0,0)) `intersect` (move_coordinates wire2 (0,0))
    putStr $ show max_int ++ "\n"
