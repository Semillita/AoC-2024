import Data.List

run :: IO ()
run = do
    fileContent <- readFile "Input1.txt"
    let linesInFile = lines fileContent
    let part1Result = part1 linesInFile
    let part2Result = part2 linesInFile
    putStrLn ("Part 1: " ++ show part1Result)
    putStrLn ("Part 2: " ++ show part2Result)

part1 :: [String] -> Int
part1 lines = differences (sort (map (!! 0) pairs)) (sort (map (!! 1) pairs))
  where
    pairs :: [[Int]] = map (map read . words) lines

differences :: [Int] -> [Int] -> Int
differences [] [] = 0
differences (x:xs) (y:ys) = abs (x - y) + differences xs ys

part2 :: [String] -> Int
part2 lines = sum (map ((\num -> num * occurrences column2 num) . (!! 0)) pairs)
  where
    pairs :: [[Int]] = map (map read . words) lines
    column2 = map (!! 1) pairs

occurrences :: [Int] -> Int -> Int
occurrences xs y = sum [1 | x <- xs, x == y]
