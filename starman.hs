check :: String -> String -> Char -> (Bool, String)
check word display c =
  ( elem c word && notElem c display,
    [ if x == c then c else y
      | (x, y) <- zip word display
    ]
  )

mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
  do
    print (display ++ " " ++ take n (repeat '*'))
    print "Enter your guess"
    q <- getLine
    let (correct, display') = check word display (q !! 0)
    let n' = if correct then n else n -1
    turn word display' n'

turn :: String -> String -> Int -> IO ()
turn word display n =
  do
    if n == 0
      then print "You Lose"
      else
        if word == display
          then print ("You win: " ++ word)
          else mkguess word display n

starman :: String -> Int -> IO ()
starman word n = turn word ['-' | x <- word] 5