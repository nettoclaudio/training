firstRepeated :: String -> String -> Char
firstRepeated a b = head [ x | x <- a, y <- b, x == y ]

firstRepeated3 :: String -> String -> String -> Char
firstRepeated3 a b c = head [ x | x <- a, y <- b, z <- c,  x == y, y == z ]

splitHalf :: String -> (String, String)
splitHalf xs =
  let
    half = length xs `div` 2
    x1 = take half xs
    x2 = drop half xs
  in
    (x1, x2)

toNumber c =
  let
    num = fromEnum c
    beginUpper = fromEnum 'A'
    endUpper   = fromEnum 'Z'
    beginLower = fromEnum 'a'
  in
    if num >= beginUpper && num <= endUpper then num - beginUpper + 27
    else num - beginLower + 1

priorityByElf []     = 0
priorityByElf (x:xs) =
  let
    (part1, part2) = splitHalf x
    repeated       = firstRepeated part1 part2
    num            = toNumber repeated
  in
    priorityByElf xs + num

priorityByGroup xs
  | length xs < 3 = 0
  | otherwise     = priorityByGroup others + toNumber (firstRepeated3 elf1 elf2 elf3)
  where (elf1:elf2:elf3:others) = xs

main = do
  contents <- getContents
  let rucksacks = words contents

  putStrLn $ "Part 1: " ++ show (priorityByElf rucksacks)
  putStrLn $ "Part 2: " ++ show (priorityByGroup rucksacks)
