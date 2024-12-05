import Data.List

run :: IO ()
run = do
    fileContent <- readFile "Input4.txt"
    let linesInFile = lines fileContent
    let part1Result = part1 linesInFile
    let part2Result = part2 linesInFile
    putStrLn ("Part 1: " ++ show part1Result)
    putStrLn ("Part 2: " ++ show part2Result)

part1 :: [String] -> Int
part1 lines = sum (map (horizontal . unwords) variations) + sum (map countDiagonal variations)
  where
    variations = [lines, lines2, lines3, lines4]
    lines2 = rotate lines
    lines3 = rotate lines2
    lines4 = rotate lines3

horizontal :: String -> Int
horizontal "" = 0
horizontal ('X':('M':('A':('S':rest)))) = 1 + horizontal rest
horizontal (_:rest) = horizontal rest

countDiagonal :: [String] -> Int
countDiagonal lines = sum [1 | column <- [0..(length (head lines) - 4)], line <- [0..(length lines - 4)], diagonalMatch lines line column]

diagonalMatch :: [String] -> Int -> Int -> Bool
diagonalMatch lines line column =
    (lines!!line)!!column == 'X'
    && (lines!!(line + 1))!!(column + 1) == 'M'
    && (lines!!(line + 2))!!(column + 2) == 'A'
    && (lines!!(line + 3))!!(column + 3) == 'S'

part2 :: [String] -> Int
part2 lines = sum (map countCrosses variations)
  where
    lines2 = rotate lines
    lines3 = rotate lines2
    lines4 = rotate lines3
    variations = [lines, lines2, lines3, lines4]

countCrosses :: [String] -> Int
countCrosses lines = sum [1 | line <- [0..(length lines - 3)], column <- [0..(length (head lines) - 3)], crossMatch lines line column]

crossMatch :: [String] -> Int -> Int -> Bool
crossMatch lines line column =
    line1!!column == 'M' && line1!!(column + 2) == 'M'
    && (lines!!(line + 1))!!(column + 1) == 'A'
    && line3!!column == 'S' && line3!!(column + 2) == 'S'
  where
    [line1, line2, line3] = [lines!!line, lines!!(line + 1), lines!!(line + 2)]

rotate :: forall a. [[a]] -> [[a]]
rotate matrix = reverse (transpose matrix)