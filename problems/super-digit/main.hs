-- Calculate the super digit of really big numbers.
superDigit :: (Integral a) => a -> a
superDigit x
  | x < 10    = x
  | otherwise = superDigit $ sum $ digits x

-- Convert a number to int array where each item corresponds
-- to number digit. Returned array in reverse order.
digits :: (Integral a) => a -> [a]
digits n
  | n == 0    = []
  | otherwise = r : digits q
  where q = div n 10
        r = mod n 10

-- Convert a list of strings to ints.
--
-- NOTE: using Integer since n < 10e100000 and k <= 10e5
str2int :: [String] -> [Integer]
str2int = map read -- more succinct than: map (\x -> read x)

main :: IO ()
main = do
  line <- getLine
  let [n, k] = str2int $ words line
  print $ superDigit $ (sum $ digits n) * k
