speller :: [[Char]] -> [Char]
speller (w : ws)
  | w : ws == [] = ""
  | w : ws == w : [] = " and " ++ [w !! 0] ++ " is for " ++ w
  | w : ws == w : ws = [w !! 0] ++ " is for " ++ w ++ ", " ++ speller ws