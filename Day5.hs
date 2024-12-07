import Data.List
import Data.Maybe

run :: IO ()
run = do
    fileContent <- readFile "Input5.txt"
    let linesInFile = lines fileContent
    let part1Result = part1 linesInFile
    let part2Result = part2 linesInFile
    putStrLn ("Part 1: " ++ show part1Result)
    putStrLn ("Part 2: " ++ show part2Result)

part1 :: [String] -> Int
part1 lines = sum (map middleNumber (filter (`validateUpdate` rules) updates))
  where
    (rules, updates) = splitInput lines False ([], [])

splitInput :: [String] -> Bool -> ([(Int, Int)], [[Int]]) -> ([(Int, Int)], [[Int]])
splitInput [] _ (rules, updates) = (rules, updates)
splitInput ("":rest) _ (rules, updates) = splitInput rest True (rules, updates)
splitInput (line:rest) False (rules, updates) = splitInput rest False (rules ++ [parseRule line], updates)
splitInput (line:rest) True (rules, updates) = splitInput rest True (rules, updates ++ [parseUpdate line])

parseRule :: String -> (Int, Int)
parseRule line = (read [head line, line!!1], read [line!!3, line!!4])

parseUpdate :: String -> [Int]
parseUpdate "" = []
parseUpdate [x, y] = [read [x, y]]
parseUpdate (',':rest) = parseUpdate rest
parseUpdate (x:(y:rest)) = read [x, y]:parseUpdate rest

validateUpdate :: [Int] -> [(Int, Int)] -> Bool
validateUpdate pages = all (followsRule pages)

followsRule :: [Int] -> (Int, Int) -> Bool
followsRule pages (page1, page2)
 | isNothing maybeIndex1 || isNothing maybeIndex2 = True
 | otherwise = fromJust maybeIndex1 < fromJust maybeIndex2
  where
    maybeIndex1 = elemIndex page1 pages
    maybeIndex2 = elemIndex page2 pages

middleNumber :: [Int] -> Int
middleNumber nums = nums!!index
  where
    index = floor (fromIntegral (length nums - 1) / 2)

part2 :: [String] -> Int
part2 lines = sum (map (middleNumber . (\nums -> order nums (applicableRules rules nums))) invalidUpdates)
  where
    (rules, updates) = splitInput lines False ([], [])
    invalidUpdates = filter (not . (`validateUpdate` rules)) updates

order :: [Int] -> [(Int, Int)] -> [Int]
order [] _ = []
order (x:xs) rules
 | (not . any (\(n1, n2) -> n1 == x)) rules = order xs (exclude x rules) ++ [x]
 | otherwise = order (xs ++ [x]) rules

exclude :: Int -> [(Int, Int)] -> [(Int, Int)]
exclude num = filter (\(_, n2) -> n2 /= num)

applicableRules :: [(Int, Int)] -> [Int] -> [(Int, Int)]
applicableRules [] _ = []
applicableRules ((n1, n2):rest) pages
 | isJust maybeIndex1 && isJust maybeIndex2 = (n1, n2):applicableRules rest pages
 | otherwise = applicableRules rest pages
  where
    maybeIndex1 = elemIndex n1 pages
    maybeIndex2 = elemIndex n2 pages