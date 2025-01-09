module Main where

data Animal = Worm | Chicken | Fox | Bear | Dinosaur
    deriving (Show, Eq, Ord)

data Card = Card {animal :: Animal, quantity :: Int}
    deriving Eq

instance Show Card where
    show (Card a q)= "Card " ++ show a ++ " " ++ show q

data Winner = P1 | P2 | Draw

instance Show Winner where
    show P1 = "Player 1 has won!"
    show P2 = "Player 2 has won!"
    show Draw = "Its a draw!"

type Deck = [Card]

createDeck :: Deck
createDeck = [Card Worm 5, Card Chicken 4, Card Fox 3, Card Bear 2, Card Dinosaur 1]

checkDeck :: Card -> Deck -> Bool
checkDeck c = any isValidCard
    where
        isValidCard (Card a q) = a == animal c && q >= quantity c

updateDeck :: Card -> Deck -> Deck
updateDeck (Card a q) = filter keepCard . map updateCard
    where
        updateCard (Card a' q')
            | a == a' && q' >= q = Card a' (q' - q)
            | otherwise = Card a' q'
        keepCard (Card _ q') = q' > 0

chooseCard :: Card -> Deck -> Maybe (Card, Deck)
chooseCard c d  
    | checkDeck c d = Just (c, updateDeck c d)
    | otherwise = Nothing

battle :: Card -> Card -> Winner
battle (Card a1 q1) (Card a2 q2) 
    | a1 == a2 = result q1 q2
    | otherwise = result a1 a2
    where
        result x y = case compare x y of
            LT -> P2
            EQ -> Draw
            GT -> P1

main :: IO ()
main = putStrLn "Hello, Haskell!"
