import qualified Data.Map.Strict as Map
import Data.List.Split
import Data.List

build_child_parent :: [String] -> Map.Map String String
build_child_parent = Map.fromList . map (\x -> (last x, head x)) . map (splitOn ")")

leaf_to_root :: String -> Map.Map String String -> [String]
leaf_to_root "COM" _ = []
leaf_to_root leaf orbits = parent:leaf_to_root parent orbits
    where
        parent = (Map.!) orbits leaf

shortest_path :: String -> String -> Map.Map String String -> Int
shortest_path start end orbits = (length (san \\ you)) + (length (you \\ san))
    where
        san = leaf_to_root "SAN" orbits
        you = leaf_to_root "YOU" orbits

main :: IO()
main = do
    contents <- readFile "data/day6.txt"
    let orbits = lines contents
    let result = shortest_path "SAN" "YOU" (build_child_parent orbits)
    putStr $ (show result) ++ "\n"
