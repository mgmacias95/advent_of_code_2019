import qualified Data.Map.Strict as Map
import Data.List.Split
import Data.List

n_levels :: String -> Int -> Map.Map String [String] -> Int
n_levels curr depth orbits = case children of [] -> depth
                                              xs -> depth + (sum $ map (\x -> n_levels x (depth+1) orbits) xs)
    where
        children = Map.findWithDefault [] curr orbits

build_parent_child :: [String] -> Map.Map String [String]
build_parent_child = Map.fromListWith (++) . map (\x -> (head x, [last x])) . map (splitOn ")")

main :: IO()
main = do
    contents <- readFile "data/day6.txt"
    let orbits = lines contents
    let result = n_levels "COM" 0 $ build_parent_child orbits
    putStr $ (show result) ++ "\n"
