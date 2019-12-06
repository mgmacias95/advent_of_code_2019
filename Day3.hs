import System.IO
import Data.List.Split
import Data.List
import Data.String
import Data.Maybe


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


n_steps :: [(Int, Int)] -> [(Int, Int)] -> Int
n_steps w1 w2 = minimum $ map (\x -> fromJust (elemIndex x w1) + fromJust (elemIndex x w2)) i
    where
        i = tail $ w1 `intersect` w2

main :: IO()
main = do
    contents <-  readFile "data/day3.txt"
    let wire1:wire2:[] = map (splitOn ",") $ lines contents
    let max_int = n_steps (move_coordinates wire1 (0,0)) (move_coordinates wire2 (0,0))
    putStr $ show max_int ++ "\n"
