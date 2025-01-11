module Main where

import System.Random
import Data.List

data Animal = Worm | Chicken | Fox | Bear | Dinosaur
    deriving (Show, Eq, Ord, Enum, Bounded)

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

player1Deck :: Deck
player1Deck = createDeck

player2Deck :: Deck
player2Deck = createDeck

printDeck :: Deck -> IO()
printDeck d = do
    putStrLn "Current deck: "
    mapM_ print d

drawCard :: IO Card
drawCard = do
    animal <- randomRIO (fromEnum (minBound :: Animal), fromEnum(maxBound :: Animal))
    return $ Card (toEnum animal) 1

addCardToDeck :: Card -> Deck -> Deck
addCardToDeck (Card a q) d = 
    case lookupCard a d of
        Just (Card _ q') -> map (\c -> if animal c == a then Card a (q + q') else c) d
        Nothing -> Card a q : d
    where
        lookupCard a' = find (\c -> animal c == a')

play :: Deck -> IO Card
play d = do
    printDeck d
    putStr "Please choose your card: "
    cardChoice <- getLine 
    case parseCard cardChoice of
        Just c -> if quantity c > 0 && checkDeck c d then return c else putStrLn "Invalid card, please try again." >> play d
        _ -> putStrLn "Invalid input, please try again." >> play d

parseCard :: String -> Maybe Card
parseCard input = case words input of
    ["Card", "Worm", q] -> Just (Card Worm (read q))
    ["Card", "Chicken", q] -> Just (Card Chicken (read q))
    ["Card", "Fox", q] -> Just (Card Fox (read q))
    ["Card", "Bear", q] -> Just (Card Bear (read q))
    ["Card", "Dinosaur", q] -> Just (Card Dinosaur (read q))
    _ -> Nothing

game :: Deck -> Deck -> IO()
game p1d p2d = do
    putStrLn "Player 1's turn: "
    let p1Move = Card Chicken 4
    putStrLn $ "Player 1 chose " ++ show p1Move
    
    putStrLn "Player 2's turn: "
    p2Move <- play p2d
    printDeck (updateDeck p2Move p2d)
    putStrLn $ "Battle result: " ++ show (battle p1Move p2Move)

    case battle p1Move p2Move of 
        P1 -> do
            drawnCard <- drawCard
            let newP1D = addCardToDeck drawnCard p1d
            game newP1D (updateDeck p2Move p2d)
        P2 -> do 
            drawnCard <- drawCard
            putStrLn $ "Player 2 draws " ++ show drawnCard
            let newP2D = addCardToDeck drawnCard (updateDeck p2Move p2d)
            game p1d newP2D
        Draw -> game p1d (updateDeck p2Move p2d)

main :: IO ()
main = game player1Deck player2Deck
