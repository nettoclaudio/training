countRangeFullyOverlaped :: [((Int, Int), (Int, Int))] -> Int
countRangeFullyOverlaped intervals =
  length $ filter (\i -> isIntevalFullyOverlapped (fst i) (snd i)) intervals

countRangeOverlapped :: [((Int, Int), (Int, Int))] -> Int
countRangeOverlapped intervals =
  length $ filter (\i -> isIntervalOverlapped (fst i) (snd i)) intervals

isIntevalFullyOverlapped :: (Int, Int) -> (Int, Int) -> Bool
isIntevalFullyOverlapped start end =
  let
    (x1, x2) = start
    (y1, y2) = end
  in
    (x1 <= y1 && x2 >= y2) || (y1 <= x1 && y2 >= x2)

isIntervalOverlapped :: (Int, Int) -> (Int, Int) -> Bool
isIntervalOverlapped start end =
  let
    (x1, x2) = start
    (y1, y2) = end
  in
    (x1 <= y1 && x2 >= y1) || (x1 <= y2 && x2 >= y2) ||  -- y is beetwen x1 and x2
    (y1 <= x1 && y2 >= x1) || (y1 <= x2 && y2 >= x2)     -- x is beetwen y1 and y2

assigmentStrToInterval :: String -> ((Int, Int), (Int, Int))
assigmentStrToInterval assignment =
  let
    [start, end] = split assignment ','
    [x1, x2]     = split start '-'
    [y1, y2]     = split end '-'
    (ix1, ix2)   = (read x1 :: Int, read x2 :: Int)
    (iy1, iy2)   = (read y1 :: Int, read y2 :: Int)
  in
    ((ix1, ix2), (iy1, iy2))

split :: String -> Char -> [String]
split []   _ = []
split text sep =
  let
    word = takeWhile (/= sep) text
    rest = drop (length word + 1) text
  in
    word : split rest sep

main = do
  contents <- getContents
  let assignments = words contents
  let intervals = map assigmentStrToInterval assignments

  putStrLn $ "Part 1: " ++ show (countRangeFullyOverlaped intervals)
  putStrLn $ "Part 2: " ++ show (countRangeOverlapped intervals)
