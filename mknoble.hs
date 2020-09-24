mknoble :: Bool -> String -> String
mknoble is_male name = (if is_male then "Sir" else "Madam") ++ " " ++ name