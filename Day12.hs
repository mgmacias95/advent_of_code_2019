import Data.List.Split (splitOn)
import Data.List (concat, lines, last)

data Point = Point {x :: Int, y :: Int, z :: Int} deriving (Show, Eq, Ord)

instance Read Point where
    readsPrec _ [] = []
    readsPrec _ string_to_parse = [(Point x y z, concat therest)]
        where
            current:therest = splitOn ">" string_to_parse
            x:y:z:[] = map (read . last . splitOn "=") $ splitOn "," current :: [Int]

infixr 5 #
(#) :: Point -> Point -> Point
(Point x y z) # (Point x' y' z') = (Point (new x x') (new y y') (new z z'))
    where
        new a b
            | a > b     = -1
            | a < b     = 1
            | otherwise = 0

infixr 5 .+
(.+) :: Point -> Point -> Point
(Point x y z) .+ (Point x' y' z') = (Point (x + x') (y + y') (z + z'))

data Planet = Planet {position :: Point, velocity :: Point} deriving (Show, Eq, Ord, Read)

single_iteration :: Planet -> [Planet] -> Planet
single_iteration (Planet pos vel) planets = Planet next_pos next_vel
    where
        next_vel = foldl (.+) vel $ map (\(Planet pos' _) -> pos # pos') planets
        next_pos = pos .+ next_vel

iteration :: [Planet] -> [Planet]
iteration planets = map (\p -> single_iteration p planets) planets

simulator :: Int -> [Point] -> [Planet]
simulator n points = last $ take (n+1) $ iterate iteration planets
    where
        planets = map (\x -> Planet x (Point 0 0 0)) points

energy :: [Planet] -> Int
energy = sum . map (\(Planet pos vel) -> (sum_point pos) * (sum_point vel))
    where
        sum_point (Point x y z) = abs x + abs y + abs z

main :: IO()
main = do
    contents <- readFile "data/day12.txt"
    let points = map (read) $ lines contents :: [Point]
    let result = energy $ simulator 1000 points
    putStrLn $ show result
