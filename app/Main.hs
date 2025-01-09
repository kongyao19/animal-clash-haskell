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
checkDeck c d = any isValidCard d
    where
        isValidCard (Card a q) = a == animal c && q >= quantity c

chooseCard :: Card -> Deck -> Maybe Card
chooseCard c d = case checkDeck c d of
    True -> Just c
    False -> Nothing 
    
battle :: Card -> Card -> Ordering
battle (Card a1 q1) (Card a2 q2)
    | a1 == a2 = compare q1 q2
    | otherwise = compare a1 a2

result :: Ordering -> Winner
result LT = P2
result EQ = Draw
result GT = P1

clash :: Card -> Card -> Winner 
clash c1 c2 = result (battle c1 c2)

main :: IO ()
main = putStrLn "Hello, Haskell!"
