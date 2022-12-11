top :: [Int] -> Int
top numbers = topk 1 numbers !! 0

topk :: Int -> [Int] -> [Int]
topk k numbers = take k $ sort numbers

sort :: [Int] -> [Int]
sort numbers
  | null numbers = []
  | otherwise    = lesser ++ [pivot] ++ greater
  where (pivot:numbers') = numbers
        greater          = sort [ n | n <- numbers', n <= pivot ]
        lesser           = sort [ n | n <- numbers', n > pivot ]

aggregatePositiveNumbers :: [Int] -> [Int] -> [Int]
aggregatePositiveNumbers numbers ps
  | null partition = ps
  | otherwise      = aggregatePositiveNumbers next (sum':ps)
  where partition = takeWhile (>0) numbers
        next      = drop (length partition + 1) numbers
        sum'      = sum partition

main = do
  contents <- getContents
  let numbers = map (\w -> if null w then -1 else read w :: Int) (lines contents)
  let calories = aggregatePositiveNumbers numbers []

  putStrLn $ "Part 1: " ++ show (top calories)
  putStrLn $ "Part 2: " ++ show (sum $ topk 3 calories)
