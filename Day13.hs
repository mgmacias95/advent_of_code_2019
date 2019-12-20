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

new_input :: Int -> Int -> Int
new_input px bx = if px < bx then 1 else (if px > bx then (-1) else 0)

next_input :: [Int] -> Int -> (Int,Int) -> (Int,Int) -> ([Int], [Int], Int, (Int,Int), (Int,Int))
next_input (e:[]) n b p = ([e], [], n, b, p)  -- incomplete output
next_input (e:f:[]) n b p = ([e,f], [], n, b, p)  -- incomplete output
next_input (bx:by:4:[]) n _ (px, py) = ([], [new_input px bx], n, (bx, by), (px, py))
next_input (px:py:3:[]) n (bx, by) (_, _) = ([], [new_input px bx], n, (bx, by), (px, py))
next_input ((-1):0:s:[]) n (bx, by) (px, py) = ([], [new_input px bx], s, (bx, by), (px, py))
next_input _ n (bx, by) (px, py) = ([], [new_input px bx], n, (bx, by), (px, py))


run_program :: [Int] -> Int -> Int -> [Int] -> (Int,Int) -> (Int,Int) -> Int -> [Int] -> ([Int], Int)
run_program input position rbase output ball paddle n program = case (d*10 + e) of 99 -> (output, n)
                                                                                   1  -> run_program input (position + 4) rbase output ball paddle n (update_list program (v1 + v2) v3)
                                                                                   2  -> run_program input (position + 4) rbase output ball paddle n (update_list program (v1 * v2) v3)
                                                                                   3  -> run_program (tail input) (position + 2) rbase output ball paddle n (update_list program (head input) ii1)
                                                                                   4  -> run_program n_input (position + 2) rbase n_output n_ball n_paddle n_n program
                                                                                   5  -> run_program input (if v1 == 0 then (position + 3) else v2) rbase output ball paddle n program
                                                                                   6  -> run_program input (if v1 /= 0 then (position + 3) else v2) rbase output ball paddle n program
                                                                                   7  -> run_program input (position + 4) rbase output ball paddle n (update_list program (if v1 < v2 then 1 else 0) v3)
                                                                                   8  -> run_program input (position + 4) rbase output ball paddle n (update_list program (if v1 == v2 then 1 else 0) v3)
                                                                                   9  -> run_program input (position + 2) (v1 + rbase) output ball paddle n program
    where
        (_, ptail) = splitAt position program
        a:b:c:d:e:[] = zfill 5 $ list_digits $ head ptail
        i0:i1:[] = take 2 ptail
        _:_:i2:i3:[] = take 4 ptail
        v1 = parameter_mode c i1 rbase program
        v2 = parameter_mode b i2 rbase program
        v3 = if (a == 2) then (i3 + rbase) else i3
        ii1 = if (c == 2) then (i1 + rbase) else i1
        (n_output, n_input, n_n, n_ball, n_paddle) = next_input (output ++ [v1]) n ball paddle

int_to_game_char :: Int -> Char
int_to_game_char x = case x of 0 -> ' '
                               1 -> '|'
                               2 -> '#'
                               3 -> '_'
                               4 -> '*'

board_print :: [Int] -> String
board_print program = unlines $ map (map (int_to_game_char)) matrix
    where
        empty_matrix = replicate 50 $ replicate 25 0
        chunks = chunksOf 3 $ fst $ run_program [] 0 0 [] (0,0) (0,0) 0 program
        world = Map.fromList $ map (\(x:y:t:[]) -> ((y,x),t)) chunks
        matrix = map (\r -> map (\c -> Map.findWithDefault 0 (r, c) world) [0..50]) [0..25]

play :: [Int] -> Int
play program = snd $ run_program [0] 0 0 [] (head ball, last ball) (head paddle, last paddle) 0 (update_list program 2 0)
    where
        chunks = chunksOf 3 $ fst $ run_program [] 0 0 [] (0,0) (0,0) 0 program
        ball = head $ filter (\x -> last x == 4) chunks
        paddle = head $ filter (\x -> last x == 3) chunks

main :: IO()
main = do
    contents <- readFile "data/day13.txt"
    let program = map read $ splitOn "," contents :: [Int]
    let result = play program
    putStr $ show result ++ "\n"
