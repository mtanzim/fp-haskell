speller_with_idx :: [[Char]] -> Int -> Int -> [Char]
speller_with_idx [] _ _ = []
speller_with_idx (w : ws) idx len
  | w : ws == w : [] = (if idx == 0 then "" else " and ") ++ [w !! 0] ++ " is for " ++ w
  | w : ws == w : ws = [w !! 0] ++ " is for " ++ w ++ (if idx == len -2 then "" else ", ") ++ speller_with_idx ws (idx + 1) len

speller :: [[Char]] -> [Char]
speller ws = speller_with_idx ws 0 (length ws)
