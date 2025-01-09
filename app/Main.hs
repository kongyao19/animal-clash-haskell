module Main where

data Animal = Worm | Chicken | Fox | Bear | Dinosaur
    deriving (Show, Eq, Ord)

data Card = Card {animal :: Animal, quantity :: Int}
    deriving (Show, Eq)

data Winner = P1 | P2 | Draw
    deriving Show

main :: IO ()
main = putStrLn "Hello, Haskell!"
