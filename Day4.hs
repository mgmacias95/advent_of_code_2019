import Data.Char

list_digits :: Int -> [Int]
list_digits = map (fromIntegral . digitToInt) . show

meets_criteria :: (Int, Int) -> Int -> Bool
meets_criteria (minc, maxc) n = (length $ show n) == 6 && (minc <= n && n <= maxc) && (and $ map (\(x,y) -> x <= y) pairs) && (or $ map (\(x,y) -> x == y) pairs)
    where
        d = list_digits n
        pairs = zip d (tail d)

how_many_meet :: [Int] -> Int
how_many_meet candidates = length $ filter (\x -> x == True) $ map (meets_criteria (minc, maxc)) candidates
        where
            minc = head candidates
            maxc = last candidates

main :: IO()
main = do
    let candidates = [353096..843212]
    let howmany = how_many_meet candidates
    putStr $ show howmany ++ "\n"
