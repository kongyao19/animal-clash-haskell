{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

data Animal = Worm | Chicken | Fox | Bear | Dinosaur
    deriving (Show, Eq, Ord)

data Card = Card {animal :: Animal, quantity :: Int}
    deriving (Show, Eq)

data Winner = P1 | P2 | Draw
    deriving Show

type Deck = [Card]

createDeck :: Deck
createDeck = [Card Worm 5, Card Chicken 4, Card Fox 3, Card Bear 2, Card Dinosaur 1]

checkDeck :: Card -> Deck -> Bool
checkDeck c = any isValidCard
    where
        isValidCard (Card a q) = a == animal c && q >= quantity c

chooseCard :: Card -> Deck -> Maybe Card
chooseCard c d = if checkDeck c d then Just c else Nothing

clash :: Card -> Card -> Winner
clash (Card a1 q1) (Card a2 q2) 
    | a1 == a2 = case compare q1 q2 of
        LT -> P2
        EQ -> Draw
        GT -> P1
    | otherwise = case compare a1 a2 of
        LT -> P2
        GT -> P1

main :: IO ()
main = putStrLn "Hello, Haskell!"
