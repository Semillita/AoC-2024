import Text.Regex
import Data.Maybe

mulRegex = mkRegex "mul\\(([0-9]+),([0-9]+)\\)"
combinedRegex = mkRegex "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)"

run :: IO ()
run = do
    fileContent <- readFile "Input3.txt"
    let linesInFile = lines fileContent
    let part1Result =  part1 linesInFile
    let part2Result = part2 linesInFile
    putStrLn ("Part 1: " ++ show part1Result)
    putStrLn ("Part 2: " ++ show part2Result)

part1 :: [String] -> Int
part1 lines = sum (map ((\pair -> head pair * pair!!1) . map read . snd) (allMatches mulRegex (concat lines)))

allMatches :: Regex -> String -> [(String, [String])]
allMatches regex source = handleMatchResult regex (matchRegexAll regex source)

handleMatchResult :: Regex -> Maybe (String, String, String, [String]) -> [(String, [String])]
handleMatchResult regex (Just (before, match, after, subExpressions)) = (match, subExpressions):allMatches regex after
handleMatchResult _ Nothing = []

part2 :: [String] -> Int
part2 lines = evaluate (map classify (allMatches combinedRegex (concat lines))) True

classify :: (String, [String]) -> Match
classify (match, subExpressions)
 | not (null (head subExpressions)) = Mul (read (head subExpressions)) (read (subExpressions!!1))
 | length match == 4 = Do
 | otherwise = Dont

evaluate :: [Match] -> Bool -> Int
evaluate [] _ = 0
evaluate (Do:xs) _ = evaluate xs True
evaluate (Dont:xs) _ = evaluate xs False
evaluate ((Mul x y):xs) True = x * y + evaluate xs True
evaluate (_:xs) _ = evaluate xs False

data Match = Mul Int Int | Do | Dont deriving Show