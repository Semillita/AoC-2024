run :: IO ()
run = do
    fileContent <- readFile "Input2.txt"
    let linesInFile = lines fileContent
    let part1Result = part1 linesInFile
    let part2Result = part2 linesInFile
    putStrLn ("Part 1: " ++ show part1Result)
    putStrLn ("Part 2: " ++ show part2Result)

part1 :: [String] -> Int
part1 input = sum [1 | report <- reports, safeReport report]
  where reports :: [[Int]] = map (map read . words) input

safeReport :: [Int] -> Bool
safeReport nums = safeIncreasing nums || safeDecreasing nums

safeIncreasing :: [Int] -> Bool
safeIncreasing [x, y] = x < y && (x + 3) >= y
safeIncreasing (x:(y:z)) = safeIncreasing [x, y] && safeIncreasing (y:z)

safeDecreasing :: [Int] -> Bool
safeDecreasing [x, y] = x > y && (x - 3) <= y
safeDecreasing (x:(y:z)) = safeDecreasing [x, y] && safeDecreasing (y:z)


part2 :: [String] -> Int
part2 input = sum [1 | report <- reports, any safeReport (variants report)]
  where reports :: [[Int]] = map (map read . words) input

variants :: [Int] -> [[Int]]
variants nums = [exclude index nums | index <- [0..(length nums - 1)]]

exclude :: Int -> [Int] -> [Int]
exclude index nums = [nums!!i | i <- [0..(length nums - 1)], i /= index]