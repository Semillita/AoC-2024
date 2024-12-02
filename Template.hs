run :: IO ()
run = do
    fileContent <- readFile "Input_.txt"
    let linesInFile = lines fileContent
    let part1Result = part1 linesInFile
    let part2Result = part2 linesInFile
    putStrLn ("Part 1: " ++ show part1Result)
    putStrLn ("Part 2: " ++ show part2Result)

part1 :: [String] -> Int
part1 _ = 0

part2 :: [String] -> Int
part2 _ = 1