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


main :: IO()
main = do
    contents <- readFile "data/day8.txt"
    let image = head $ lines contents
    let result = zeros_per_layer image (25,6)
    putStr $ (show result) ++ "\n"
