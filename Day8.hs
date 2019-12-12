import Data.List
import Data.List.Split
import Data.Maybe

zeros_per_layer :: String -> (Int, Int) -> Int
zeros_per_layer image (width, height) = (n '1' $ layers !! min_nzeros_layer) * (n '2' $ layers !! min_nzeros_layer)
    where
        layers = chunksOf (width*height) image
        nzeros = map (length . filter (== '0')) layers
        min_nzeros_layer = fromJust $ elemIndex (minimum nzeros) nzeros
        n i = length . filter (== i)

to_printeable :: Char -> Char
to_printeable x = if x == '1' then '#' else ' '

message :: String -> (Int, Int) -> String
message image (width, height) = map (to_printeable . head . filter (/= '2')) $ transpose $ chunksOf (width*height) image

main :: IO()
main = do
    contents <- readFile "data/day8.txt"
    let image = head $ lines contents
    let result = message image (25,6)
    putStrLn $ unlines $ chunksOf 25 result
