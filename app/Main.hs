module Main where

import System.Random
import Data.Char
import qualified Data.Map as Map

data Animal = Worm | Chicken | Fox | Bear | Dinosaur
    deriving (Show, Eq, Ord, Enum, Bounded)

data Card a = Card {animal :: a, quantity :: Int}
    deriving Eq

instance Show a => Show (Card a) where
    show (Card a q) = "Card " ++ show a ++ " " ++ show q

instance (Eq a, Bounded a) => Semigroup (Card a) where
    (Card a1 q1) <> (Card a2 q2)
        | q1 == 0 = Card a2 q2
        | q2 == 0 = Card a1 q1
        | a1 == a2 = Card a1 (q1 + q2)
        | otherwise = mempty

instance (Eq a, Bounded a) => Monoid (Card a) where
    mempty = Card minBound 0

instance Functor Card where
    fmap f (Card a q) = Card (f a) q

instance Applicative Card where
    pure a = Card a 1
    (Card f q) <*> (Card a q') = Card (f a) (q + q')

instance Monad Card where
    return = pure
    (Card a q) >>= f = let (Card a' q') = f a in Card a' (q + q')

data Winner = P1 | P2 | Draw

instance Show Winner where
    show P1 = "Player 1 has won!"
    show P2 = "Player 2 has won!"
    show Draw = "Its a draw!"

type Deck = [Card Animal]

createDeck :: Deck
createDeck = [Card Worm 5, Card Chicken 4, Card Fox 3, Card Bear 2, Card Dinosaur 1]

checkDeck :: Card Animal -> Deck -> Bool
checkDeck (Card a q) = any (\(Card a' q') -> a == a' && q' >= q)

removeFromDeck :: Card Animal -> Deck -> Deck
removeFromDeck (Card a q) = filter (\c -> quantity c > 0) . fmap (\c -> if animal c == a then Card a (-q) <> c else c)

addToDeck :: Card Animal -> Deck -> Deck
addToDeck c d = if checkDeck c d then fmap (\c' -> if animal c' == animal c then c <> c' else c') d else c : d

battle :: Card Animal -> Card Animal -> Winner
battle (Card Worm _) (Card Dinosaur _) = P1
battle (Card Dinosaur _) (Card Worm _) = P2
battle (Card a1 q1) (Card a2 q2)
    | a1 == a2 = result q1 q2
    | otherwise = result a1 a2
    where
        result x y = case compare x y of
            LT -> P2
            EQ -> Draw
            GT -> P1

printDeck :: Deck -> IO ()
printDeck d = do
    putStrLn "Deck: "
    mapM_ print d

drawCard :: Enum a => IO (Card a)
drawCard = do
    a <- randomRIO (fromEnum (minBound :: Animal), fromEnum (maxBound :: Animal))
    return $ Card (toEnum a) 1

totalCards :: Deck -> Int
totalCards = sum . map quantity

cardOrCards :: Int -> String
cardOrCards 1 = " card"
cardOrCards _ = " cards"

play :: Deck -> IO (Card Animal)
play d = do
    printDeck d
    putStr "\nPlease choose your card: Card "
    cardChoice <- getLine
    maybe (retry d) validateCard (parseCard cardChoice)
    where
        retry d' = putStrLn "\nInvalid card, please try again.\n" >> play d'
        validateCard c = if quantity c > 0 && checkDeck c d then return c else retry d

animalMap :: Map.Map String Animal
animalMap = Map.fromList [("worm", Worm), ("chicken", Chicken), ("fox", Fox), ("bear", Bear), ("dinosaur", Dinosaur)]

parseCard :: String -> Maybe (Card Animal)
parseCard input = case words input of
    [a, q] -> fmap (\animal' -> Card animal' (read q)) (Map.lookup (map toLower a) animalMap)
    _ -> Nothing

game :: Deck -> Deck -> IO ()
game p1d p2d
    | null p1d && null p2d = do
        putStrLn "Both players hold their ground! It's an epic draw!"
        _ <- getLine
        askForNewGame
    | null p1d = do
        putStrLn "Player 1 has no cards left! Player 2 emerges victorious in the Animal Clash!"
        _ <- getLine
        askForNewGame
    | null p2d = do
        putStrLn "Player 2 has no cards left! Player 1 emerges victorious in the Animal Clash!"
        _ <- getLine
        askForNewGame
    | otherwise = do
        putStrLn "\nA new round has begun. Ready, set, clash!"
        putStrLn $ "Player 1's deck: " ++ show (totalCards p1d) ++ cardOrCards (totalCards p1d) ++ " remaining."
        putStrLn $ "Player 2's deck: " ++ show (totalCards p2d) ++ cardOrCards (totalCards p2d) ++ " remaining."
        _ <- getLine
        putStrLn "Player 1's turn: \n..... \nPlayer 1 has chosen a card!"
        p1Move <- randomCard p1d
        _ <- getLine
        putStrLn "Player 2's turn: \n..... "
        p2Move <- play p2d
        putStrLn $ "\nClash! \n(P1) " ++ show p1Move ++ " vs. (P2) " ++ show p2Move
        _ <- getLine
        putStrLn $ "Battle result: " ++ show (battle p1Move p2Move)
        _ <- getLine
        case battle p1Move p2Move of
            P1 -> do
                drawnCard <- drawCard
                putStrLn "Player 1 draws a card."
                let newP1D = addToDeck drawnCard (removeFromDeck p1Move p1d)
                _ <- getLine
                game newP1D (removeFromDeck p2Move p2d)
            P2 -> do
                drawnCard <- drawCard
                putStrLn $ "Player 2 draws a " ++ show drawnCard ++ "."
                let newP2D = addToDeck drawnCard (removeFromDeck p2Move p2d)
                _ <- getLine
                printDeck newP2D
                _ <- getLine
                game (removeFromDeck p1Move p1d) newP2D
            Draw -> game (removeFromDeck p1Move p1d) (removeFromDeck p2Move p2d)

askForNewGame :: IO ()
askForNewGame = do
    putStrLn "Do you want to start a new game? (yes/no)"
    response <- getLine
    case map toLower response of
        "yes" -> game createDeck createDeck
        "no" -> putStrLn "\nThanks for playing!"
        _ -> putStrLn "\nInvalid input. Please type 'yes' or 'no'." >> askForNewGame

randomCard :: Deck -> IO (Card Animal)
randomCard d = do
    randomIndex <- randomRIO (0, length d - 1)
    let Card a maxQ = d !! randomIndex
    randomQuantity <- randomRIO (1, maxQ)
    return (Card a randomQuantity)

mainMenu :: IO ()
mainMenu = do
    putStrLn "Main Menu \n1. Start New Game - Begin a thrilling journey of Animal Clash! \n2. How to Play    - Learn the rules and master the game. \n3. View Credits   - See who made this amazing game. \n4. Exit           - Say goodbye... for now."
    putStr "Please enter your choice (1-4): "
    choice <- getLine
    handleMenuChoice choice

handleMenuChoice :: String -> IO ()
handleMenuChoice c = case c of
    "1" -> do
        putStrLn "\nStarting a new game..."
        _ <- getLine
        game createDeck createDeck
    "2" -> do
        displayRules
        _ <- getLine
        mainMenu
    "3" -> do
        displayCredits
        _ <- getLine
        mainMenu
    "4" -> putStrLn "\nThanks for playing!"
    _ -> do
        putStrLn "\nInvalid choice. Please try again."
        mainMenu

displayRules :: IO ()
displayRules = do
    putStrLn "\nHow to Play: "
    putStrLn "1. Each player starts with a deck of 15 cards."
    putStrLn "   Rank 1 (Strongest): Dinosaur - 1 card"
    putStrLn "   Rank 2            : Bear     - 2 cards"
    putStrLn "   Rank 3            : Fox      - 3 cards"
    putStrLn "   Rank 4            : Chicken  - 4 cards"
    putStrLn "   Rank 5 (Weakest)  : Worm     - 5 cards"
    putStrLn "2. Players take turns playing cards by selecting an animal type and a quantity."
    putStrLn "3. The winner of a round is determined as follows:"
    putStrLn "   - Higher-ranked animals defeat lower-ranked animals (e.g., Bear beats Fox)."
    putStrLn "   - If both players play the same animal type, the player with the higher quantity wins."
    putStrLn "   - If both the type and quantity are the same, the round is a draw."
    putStrLn "4. Exception: Worms (Rank 5) can defeat Dinosaurs (Rank 1)!"
    putStrLn "5. After each round:"
    putStrLn "   - All cards played are discarded."
    putStrLn "   - The winner draws a random card to add to their deck."
    putStrLn "   - If it's a draw, none of the players get to draw a new card."
    putStrLn "6. The game ends when one player has no cards left in their deck. The other player is declared the winner!"
    putStrLn "7. Use your strategy to manage your deck wisely and outsmart your opponent!"

displayCredits :: IO ()
displayCredits = do
    putStrLn "\n==================================="
    putStrLn "           Game Credits           "
    putStrLn "==================================="
    putStrLn "Game Title: Animal Clash"
    putStrLn "Developed by: Kong Yao"
    putStrLn "Special Thanks to:"
    putStrLn " - Dr. Chin Teck Min for guidance"
    putStrLn " - OpenAI ChatGPT for assistance"
    putStrLn " - Family and friends for support"
    putStrLn " - Haskell Community for resources"
    putStrLn "==================================="
    putStrLn "Thank you for playing Animal Clash!"
    putStrLn "==================================="

main :: IO ()
main = do
    putStrLn "\nWelcome to Animal Clash!"
    putStrLn "Prepare for an epic card battle where strategy and instincts collide. Will you rise as the ultimate champion?\n"
    mainMenu


