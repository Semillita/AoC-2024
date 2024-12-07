import Data.Set (size, insert, empty, member, Set)

run :: IO ()
run = do
    fileContent <- readFile "Input6.txt"
    let linesInFile = lines fileContent
    let part1Result = part1 linesInFile
    let part2Result = part2 linesInFile
    putStrLn ("Part 1: " ++ show part1Result)
    putStrLn ("Part 2: " ++ show part2Result)

part1 :: [String] -> Int
part1 lines = walk lines (startPos lines) (0, -1) empty

startPos :: [String] -> (Int, Int)
startPos lines = head ([(x, y) | y <- [0..(length lines - 1)], x <- [0..(length (head lines) - 1)], (lines!!y)!!x == '^'])

walk :: [String] -> (Int, Int) -> (Int, Int) -> Set (Int, Int) -> Int
walk lines (posX, posY) (dirX, dirY) visited
 | nextY < 0 || nextY >= length lines || nextX < 0 || nextX >= length (head lines) = size nextVisited
 | (lines!!nextY)!!nextX == '#' = walk lines (posX, posY) (-dirY, dirX) nextVisited
 | otherwise = walk lines (nextX, nextY) (dirX, dirY) nextVisited
  where
    (nextX, nextY) = (posX + dirX, posY + dirY)
    nextVisited = insert (posX, posY) visited

part2 :: [String] -> Int
part2 lines = sum [1 | p <- emptyPositions lines, walk2 (addObstacle lines p) (startPos lines) (0, -1) empty]

emptyPositions :: [String] -> [(Int, Int)]
emptyPositions lines = [(x, y) | y <- [0..(length lines - 1)], x <- [0..(length (head lines) - 1)], (lines!!y)!!x == '.']

addObstacle :: [String] -> (Int, Int) -> [String]
addObstacle lines (x, y) = take y lines ++ [take x line ++ "#" ++ drop (x + 1) line] ++ drop (y + 1) lines
  where
    line = lines!!y

walk2 :: [String] -> (Int, Int) -> (Int, Int) -> Set ((Int, Int), (Int, Int)) -> Bool
walk2 lines (posX, posY) (dirX, dirY) visited
 | nextY < 0 || nextY >= length lines || nextX < 0 || nextX >= length (head lines) = False
 | member ((posX, posY), (dirX, dirY)) visited = True
 | (lines!!nextY)!!nextX == '#' = walk2 lines (posX, posY) (-dirY, dirX) nextVisited
 | otherwise = walk2 lines (nextX, nextY) (dirX, dirY) nextVisited
  where
    (nextX, nextY) = (posX + dirX, posY + dirY)
    nextVisited = insert ((posX, posY), (dirX, dirY)) visited