odds :: [a] -> [a]
odds []       = []
odds (x:[])   = x : []
odds (x:_:xs) = x : odds xs

evens :: [a] -> [a]
evens []       = []
evens (_:[])   = []
evens (_:x:xs) = x : evens xs

originalRules :: Char -> Char -> Int
originalRules 'A' 'X' = 3 + 1 -- draw with rock
originalRules 'A' 'Y' = 6 + 2 -- win  with paper
originalRules 'A' 'Z' = 0 + 3 -- loss with scissor

originalRules 'B' 'X' = 0 + 1 -- loss with rock
originalRules 'B' 'Y' = 3 + 2 -- draw with paper
originalRules 'B' 'Z' = 6 + 3 -- win  with scissor

originalRules 'C' 'X' = 6 + 1 -- win  with rock
originalRules 'C' 'Y' = 0 + 2 -- loss with paper
originalRules 'C' 'Z' = 3 + 3 -- draw with scissor

cheatingRules :: Char -> Char -> Int
cheatingRules 'A' 'X' = originalRules 'A' 'Z' -- we need to lose
cheatingRules 'A' 'Y' = originalRules 'A' 'X' -- we need to draw
cheatingRules 'A' 'Z' = originalRules 'A' 'Y' -- we need to win

cheatingRules 'B' 'X' = originalRules 'B' 'X' -- we need to lose
cheatingRules 'B' 'Y' = originalRules 'B' 'Y' -- we need to draw
cheatingRules 'B' 'Z' = originalRules 'B' 'Z' -- we need to win

cheatingRules 'C' 'X' = originalRules 'C' 'Y' -- we need to lose
cheatingRules 'C' 'Y' = originalRules 'C' 'Z' -- we need to draw
cheatingRules 'C' 'Z' = originalRules 'C' 'X' -- we need to win

playRockPaperScissors :: [(Char, Char)] -> (Char -> Char -> Int) -> Int
playRockPaperScissors rounds rules = sum [ rules x y | (x, y) <- rounds ]

main :: IO ()
main = do
  contents <- getContents

  let game = map head $ words contents
  let gameByRounds = zip (odds game) (evens game)

  putStrLn $ "Part 1: " ++ show (playRockPaperScissors gameByRounds originalRules)
  putStrLn $ "Part 2: " ++ show (playRockPaperScissors gameByRounds cheatingRules)
