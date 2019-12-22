import Data.List.Split
import Data.List
import qualified Data.Map.Lazy as Map

to_tuple :: [String] -> (Int, String)
to_tuple (v:k:[]) = (read v :: Int, k)

to_elem :: [String] -> (String, (Int, [(Int, String)]))
to_elem (value:key:[]) = (kname, (read kval :: Int, vals))
    where
        kval:kname:[] = splitOn " " key
        vals = map (to_tuple . splitOn " ") $ splitOn ", " value

to_map :: [String] -> Map.Map String (Int, [(Int, String)])
to_map = Map.fromList . map (to_elem . splitOn " => ")

chemicals :: Map.Map String (Int, [(Int, String)]) -> Map.Map String Int -> [(Int, String)] -> [(Int, Map.Map String Int)]
chemicals conversions lefties [] = []
chemicals conversions lefties ((q, "ORE"):[]) = [(q, lefties)]
chemicals conversions lefties ((q, current):xs) = res ++ chemicals conversions (snd $ last res) xs
    where
        l = Map.findWithDefault 0 current lefties
        (how_many, elems) = conversions Map.! current
        division = (q - l) `div` how_many
        multiplier = if ((q - l) `mod` how_many == 0) then division else division + 1
        mult_elems = map (\(x,y) -> (multiplier * x, y)) elems
        necessary = (multiplier * how_many - q) + l
        lefties' = Map.insert current necessary lefties
        res = chemicals conversions lefties' mult_elems

n_chemicals :: [(Int, Map.Map String Int)] -> Int
n_chemicals = sum . map (fst)

main :: IO()
main = do
    contents <- readFile "data/day14.txt"
    let nc = n_chemicals $ chemicals (to_map $ lines contents) (Map.fromList []) [(1, "FUEL")]
    putStr $ show nc ++ "\n"
