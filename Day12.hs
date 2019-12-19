import Data.List.Split (splitOn)
import Data.List
import qualified Data.Set as Set

data Point = Point {x :: Int, y :: Int, z :: Int} deriving (Show, Eq, Ord)

instance Read Point where
    readsPrec _ [] = []
    readsPrec _ string_to_parse = [(Point x y z, concat therest)]
        where
            current:therest = splitOn ">" string_to_parse
            x:y:z:[] = map (read . last . splitOn "=") $ splitOn "," current :: [Int]

data Planet = Planet {position :: Point, velocity :: Point} deriving (Show, Eq, Ord, Read)

infixr 5 !!!
(!!!) :: Planet -> Int -> (Int, Int)
(Planet (Point px py pz) (Point vx vy vz)) !!! i
    | i == 0 = (px, vx)
    | i == 1 = (py, vy)
    | i == 2 = (pz, vz)

infixr 7 #
(#) :: (Int, Int) -> (Int, Int) -> Int
(x, y) # (x', y')
    | x > x'     = -1
    | x < x'     = 1
    | otherwise  = 0

steps_coord :: [(Int, Int)] -> Set.Set [(Int, Int)] -> Int
steps_coord xs s
    | Set.member next s = Set.size s
    | otherwise         = steps_coord next (Set.insert next s)
    where
        next_vel = map (\x -> foldl (+) (snd x) $ map (\y -> x # y) xs) xs
        next = map (\((x, y), v) -> (x+v, v)) $ zip xs next_vel

necessary_steps :: [Point] -> Int
necessary_steps points = foldl1 (lcm) steps_coords
    where
        planets = map (\x -> Planet x (Point 0 0 0)) points
        steps_coords = map (\x -> steps_coord (map (!!! x) planets) (Set.fromList [])) [0..2]

main :: IO()
main = do
    contents <- readFile "data/day12.txt"
    let points = map (read) $ lines contents :: [Point]
    let result = necessary_steps points
    putStrLn $ show result
