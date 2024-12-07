run :: IO ()
run = do
    fileContent <- readFile "Input7.txt"
    let linesInFile = lines fileContent
    let part1Result = part1 linesInFile
    let part2Result = part2 linesInFile
    putStrLn ("Part 1: " ++ show part1Result)
    putStrLn ("Part 2: " ++ show part2Result)

part1 :: [String] -> Int
part1 lines = sum [value | (value, operands) <- map parseLine lines, check value operands]

parseLine :: String -> (Int, [Int])
parseLine line = (read (take (length value - 1) value), map read operands)
  where (value:operands) = words line

check :: Int -> [Int] -> Bool
check result [operand] = result == operand
check result (op1:(op2:rest)) = check result (op1 + op2:rest) || check result (op1 * op2:rest)

part2 :: [String] -> Int
part2 lines = sum [value | (value, operands) <- map parseLine lines, check2 value operands]

check2 :: Int -> [Int] -> Bool
check2 result [operand] = result == operand
check2 result (op1:(op2:rest)) = 
    check2 result (op1 + op2:rest)
    || check2 result (op1 * op2:rest)
    || check2 result (read (show op1 ++ show op2):rest)