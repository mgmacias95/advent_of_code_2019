import System.IO
import Data.List

single_fuel :: (Fractional a, RealFrac a) => a -> Int
single_fuel mass
    | mass <= 0 = 0
    | otherwise = (floor $ mass / 3) - 2

total_fuel :: (Fractional a, RealFrac a) => [a] -> Int
total_fuel input = floor $ sum $ map additional_fuel input
    where
        additional_fuel m = sum $ tail $ unfoldr (\x -> if x <= 0 then Nothing else Just (x, fromIntegral $ single_fuel x)) m

main :: IO()
main = do
    contents <-  readFile "data/day1.txt"
    let numbers = map read $ lines contents :: [Float]
    let solution = total_fuel numbers
    putStr $ show solution ++ "\n"
