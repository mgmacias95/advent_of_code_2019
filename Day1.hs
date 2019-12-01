import System.IO

total_fuel :: (Fractional a, RealFrac a) => [a] -> Int
total_fuel input = sum $ map (\x -> (floor (x / 3)) - 2) input

main :: IO()
main = do
    contents <-  readFile "data/day1.txt"
    let numbers = map read $ lines contents :: [Float]
    let solution = total_fuel numbers
    putStr $ show solution ++ "\n"
