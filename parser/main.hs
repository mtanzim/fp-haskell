module Main where
data Address = MkAddress {
    line1 :: String,
    number :: Integer,
    street :: String,
    town :: String,
    postcode :: String
} deriving (Show)

data Label = Green | Red | Blue | Yellow deriving (Show)

data PersonRecord  = MkPersonRecord {
    name :: String,
    address :: Address,
    id :: Integer,
    labels :: [Label]    
} deriving (Show)

rec1 :: PersonRecord
rec1 = MkPersonRecord 
    "Wim Vanderbauwhede" 
    (MkAddress "School of Computing Science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
    557188
    [Green, Red]

rec2 :: PersonRecord
rec2 = MkPersonRecord 
    "Jeremy Singer" 
    (MkAddress "School of Computing Science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
    42
    [Blue, Yellow]

main :: IO ()
main = putStrLn $ show [rec1,rec2]    