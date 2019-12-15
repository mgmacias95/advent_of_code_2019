import Data.List

data Point = Point {x :: Int, y :: Int} deriving (Eq, Show, Ord)

coordinates :: [String] -> [Point]
coordinates asteroids = concat $ map (\(i,l) -> map (\e -> (Point e i)) l) $ zip [0..length(asteroids)] $ map (elemIndices '#') asteroids

is_between :: Point -> Point -> Point -> Bool
is_between (Point ax ay) (Point bx by) (Point cx cy) = crossproduct == 0 && coord_cmp ax bx cx && coord_cmp ay by cy
    where
        crossproduct = (cy - ay) * (bx - ax) - (cx - ax) * (by - ay)
        coord_cmp x y z = min x y <= z && z <= max x y

n_seen_asteroids :: [Point] -> Int
n_seen_asteroids points = maximum $ map (\x -> length . filter (not) . map (foldl1 (||)) $ filter (not . null) $ map (\y -> map (\(p1,p2,p3) -> is_between p1 p2 p3) $ (filter (\(p1,p2,p3) -> p1 /= p2 && p1 /= p3 && p3 /= p2) $ map (\z -> (x,y,z)) points)) points) points

main :: IO()
main = do
    contents <- readFile "data/day10.txt"
    let map = lines contents
    let result = n_seen_asteroids $ coordinates map
    putStrLn $ show result ++ "\n"
