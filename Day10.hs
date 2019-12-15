import Data.List
import Data.Maybe

data Point = Point {x :: Int, y :: Int} deriving (Eq, Show, Ord)

coordinates :: [String] -> [Point]
coordinates asteroids = concat $ map (\(i,l) -> map (\e -> (Point e i)) l) $ zip [0..length(asteroids)] $ map (elemIndices '#') asteroids

angle :: Point -> Point -> Float
angle (Point px py) (Point qx qy) = 90 + (180 / pi) * (atan2 (fromIntegral (qy - py)) (fromIntegral (qx - px)))

distance :: Point -> Point -> Int
distance (Point px py) (Point qx qy) = abs (px - qx) + abs (py - qy)

sort_by_distance :: Point -> [Point] -> [Point]
sort_by_distance p points = map (fst) $ sortBy (\(_, a) (_, a') -> compare a a') $ zip points $ map (\p' -> distance p p') points

sort_by_angle :: Point -> [Point] -> [Point]
sort_by_angle p points = map (fst) $ take (length points) $ dropWhile (\(_, a) -> a < 0) $ cycle all_sort
    where
        all_sort = sortBy (\(_, a) (_, a') -> compare a a') $ zip points $ map (\p' -> angle p p') points

is_between :: Point -> Point -> Point -> Bool
is_between (Point ax ay) (Point bx by) (Point cx cy) = crossproduct == 0 && coord_cmp ax bx cx && coord_cmp ay by cy
    where
        crossproduct = (cy - ay) * (bx - ax) - (cx - ax) * (by - ay)
        coord_cmp x y z = min x y <= z && z <= max x y

point_combinations :: Point -> [Point] -> [Bool]
point_combinations p points = map (\x -> foldl (||) False $ map (\(p1,p2,p3) -> is_between p1 p2 p3) $ filter (\(p1,p2,p3) -> p2 /= p3) $ map (\y -> (p,x,y)) points_p) points_p
    where
        points_p = filter (/= p) points

n_seen_asteroids :: [Point] -> [Int]
n_seen_asteroids points = map (\p -> length . filter (not) $ point_combinations p points) points

best_location :: [Point] -> Point
best_location points = points !! i
    where
        seen = n_seen_asteroids points
        i = fromJust $ elemIndex (maximum seen) seen

destroyed_asteroids :: Point -> [Point] -> [Point]
destroyed_asteroids _ [] = []
destroyed_asteroids (Point best_x best_y) points = nexts ++ (destroyed_asteroids (Point best_x best_y) (points \\ nexts))
    where
        nexts = map (fst) $ filter (\(_,x) -> not x) $ zip points $ point_combinations (Point best_x best_y) points

find_200 :: [Point] -> Point
find_200 points = p !! 200
    where
        best = best_location points
        sort_distance = sort_by_distance best points
        p = destroyed_asteroids best $ sort_by_angle best sort_distance

main :: IO()
main = do
    contents <- readFile "data/day10.txt"
    let asteroids = lines contents
    let result = find_200 $ coordinates asteroids
    putStrLn $ show result ++ "\n"
