module Main where

import System.Random
import Data.Char
import qualified Data.Map as Map
import Data.Foldable

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
    pure a = Card a 0
    (Card f q) <*> (Card a q') = Card (f a) (q + q')

instance Monad Card where
    return = pure
    (Card a q) >>= f = let (Card a' q') = f a in Card a' (q + q')

data Winner = P1 | P2 | Draw

instance Show Winner where
    show P1 = "Player 1 has won!"
    show P2 = "Player 2 has won!"
    show Draw = "Its a draw!"

data GameState = GameState {p1Score :: Int, p2Score :: Int}

type Deck = [Card Animal]

initialGS :: GameState
initialGS = GameState 0 0

updateScore :: Winner -> GameState -> GameState
updateScore P1 (GameState p1 p2) = GameState (p1 + 1) p2
updateScore P2 (GameState p1 p2) = GameState p1 (p2 + 1)
updateScore Draw gs = gs

viewScore :: GameState -> IO ()
viewScore (GameState p1 p2) = 
    putStrLn "\n===========================================" >>
    putStrLn "                 SCOREBOARD           " >>
    putStrLn "===========================================" >>
    putStrLn ("Player 1: " ++ show p1 ++ " points   |   Player 2: " ++ show p2 ++ " points ") >>
    putStrLn "==========================================="

createDeck :: Deck
createDeck = [Card Worm 5, Card Chicken 4, Card Fox 3, Card Bear 2, Card Dinosaur 1]

checkDeck :: Card Animal -> Deck -> Bool
checkDeck (Card a q) = any (\(Card a' q') -> a == a' && q' >= q)

removeFromDeck :: Card Animal -> Deck -> Deck
removeFromDeck (Card a q) = filter (\c -> quantity c > 0) . fmap (\c -> if animal c == a then Card a (-q) <> c else c)

addToDeck :: Card Animal -> Deck -> Deck
addToDeck c d = if checkDeck c d then (\c' -> if animal c' == animal c then c <> c' else c') <$> d else c : d

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
printDeck d =
    putStrLn "Deck: " >>
    traverse_ print d

drawCard :: Enum a => IO (Card a)
drawCard =
    randomRIO (fromEnum (minBound :: Animal), fromEnum (maxBound :: Animal)) >>=
    \a -> return $ Card (toEnum a) 1

totalCards :: Deck -> Int
totalCards = sum . fmap quantity

cardOrCards :: Int -> String
cardOrCards 1 = " card"
cardOrCards _ = " cards"

randomCard :: Deck -> IO (Card Animal)
randomCard d =
    randomRIO (0, length d - 1) >>=
    \randomIndex -> let Card a maxQ = d !! randomIndex in randomRIO (1, maxQ) >>=
    \randomQuantity -> return (Card a randomQuantity)

upgradeDeck :: Deck -> Deck
upgradeDeck = fmap (upgradeCard <*>)
    where
        upgradeCard = pure (\a -> if a == Dinosaur then Worm else succ a)

promptUpgrade :: Deck -> IO (Bool, Deck)
promptUpgrade d =
    printDeck d >>
    putStrLn "\nDo you wish to buff your cards now? (yes/no)" >>
    getLine >>= \response ->
    case map toLower response of
        "yes" -> return (True, upgradeDeck d)
        "no" -> return (False, d)
        _ -> putStrLn "\nInvalid input. Please type 'yes' or 'no'." >> promptUpgrade d

play :: Deck -> IO (Card Animal)
play d =
    printDeck d >>
    putStr "\nPlease choose your card: Card " >>
    getLine >>= \cardChoice ->
    maybe (retry d) validateCard (parseCard cardChoice)
    where
        retry d' = putStrLn "\nInvalid card, please try again.\n" >> play d'
        validateCard c = if quantity c > 0 && checkDeck c d then return c else retry d

animalMap :: Map.Map String Animal
animalMap = Map.fromList [("worm", Worm), ("chicken", Chicken), ("fox", Fox), ("bear", Bear), ("dinosaur", Dinosaur)]

parseCard :: String -> Maybe (Card Animal)
parseCard input = case words input of
    [a, q] -> (\animal' -> Card animal' (read q)) <$> Map.lookup (map toLower a) animalMap
    _ -> Nothing

game :: Deck -> Deck -> Bool -> GameState -> IO ()
game p1d p2d u gs
    | null p1d && null p2d = endGame "Both players hold their ground! It's an epic draw!" gs
    | null p1d = endGame "Player 1 has no cards left! Player 2 emerges victorious in the Animal Clash!" gs
    | null p2d = endGame "Player 2 has no cards left! Player 1 emerges victorious in the Animal Clash!" gs
    | otherwise = 
        putStrLn "\nA new round has begun. Ready, set, clash!" >>
        putStrLn ("Player 1's deck: " ++ show (totalCards p1d) ++ cardOrCards (totalCards p1d) ++ " remaining.") >>
        putStrLn ("Player 2's deck: " ++ show (totalCards p2d) ++ cardOrCards (totalCards p2d) ++ " remaining.") >>
        getLine >>
        putStrLn "Player 1's turn: \n..... \nPlayer 1 has chosen a card!" >>
        randomCard p1d >>= \p1Move ->
        getLine >>
        putStrLn "Player 2's turn: \n..... " >>
        chooseP2Move u p2d >>= \(upgradeUsed, p2MoveIO, updatedDeck) -> 
        p2MoveIO >>= \p2Move -> 
        putStrLn ("\nClash! \n(P1) " ++ show p1Move ++ " vs. (P2) " ++ show p2Move) >>
        getLine >>
        putStrLn ("Battle result: " ++ show (battle p1Move p2Move)) >>
        getLine >>
        let updatedGS = updateScore (battle p1Move p2Move) gs 
        in case battle p1Move p2Move of
            P1 -> 
                drawCard >>= \drawnCard -> 
                let newP1D = addToDeck drawnCard (removeFromDeck p1Move p1d) in
                putStrLn "Player 1 draws a card." >>
                getLine >>
                game newP1D (removeFromDeck p2Move updatedDeck) upgradeUsed updatedGS
            P2 -> 
                drawCard >>= \drawnCard ->
                let newP2D = addToDeck drawnCard (removeFromDeck p2Move updatedDeck) in
                putStrLn ("Player 2 draws a " ++ show drawnCard ++ ".") >>
                getLine >>
                printDeck newP2D >>
                getLine >>
                game (removeFromDeck p1Move p1d) newP2D upgradeUsed updatedGS
            Draw -> game (removeFromDeck p1Move p1d) (removeFromDeck p2Move updatedDeck) upgradeUsed updatedGS

endGame :: String -> GameState -> IO ()
endGame message gs = putStrLn message >> getLine >> viewScore gs >> getLine >> askForNewGame

askForNewGame :: IO ()
askForNewGame = 
    putStrLn "Do you want to start a new game? (yes/no)" >>
    getLine >>= \response ->
    case map toLower response of
        "yes" -> game createDeck createDeck False initialGS
        "no" -> putStrLn "\nThanks for playing!"
        _ -> putStrLn "\nInvalid input. Please type 'yes' or 'no'." >> askForNewGame

chooseP2Move :: Bool -> Deck -> IO (Bool, IO (Card Animal), Deck)
chooseP2Move u p2d
    | u = play p2d >>= \p2Move -> return (u, return p2Move, p2d)
    | otherwise = 
        promptUpgrade p2d >>= \(upgradeUsed, updatedDeck) -> 
        putStrLn "" >>
        play updatedDeck >>= \p2Move ->
        return (upgradeUsed, return p2Move, updatedDeck)

mainMenu :: IO ()
mainMenu = 
    putStrLn "Main Menu \n1. Start New Game - Begin a thrilling journey of Animal Clash! \n2. How to Play    - Learn the rules and master the game. \n3. View Credits   - See who made this amazing game. \n4. Exit           - Say goodbye... for now." >>
    putStr "Please enter your choice (1-4): " >>
    getLine >>= handleMenuChoice 

handleMenuChoice :: String -> IO ()
handleMenuChoice c = case c of
    "1" -> putStrLn "\nStarting a new game..." >> getLine >> game createDeck createDeck False initialGS
    "2" -> displayRules >> getLine >> mainMenu
    "3" -> displayCredits >> getLine >> mainMenu
    "4" -> putStrLn "\nThanks for playing!"
    _ -> putStrLn "\nInvalid choice. Please try again." >> mainMenu

displayRules :: IO ()
displayRules = putStrLn $ unlines
    [ "\nHow to Play:"
    , "1. Each player starts with a deck of 15 cards."
    , "   Rank 1 (Strongest): Dinosaur - 1 card"
    , "   Rank 2            : Bear     - 2 cards"
    , "   Rank 3            : Fox      - 3 cards"
    , "   Rank 4            : Chicken  - 4 cards"
    , "   Rank 5 (Weakest)  : Worm     - 5 cards"
    , "2. Players take turns playing cards by selecting an animal type and a quantity."
    , "3. The winner of a round is determined as follows:"
    , "   - Higher-ranked animals defeat lower-ranked animals (e.g., Bear beats Fox)."
    , "   - If both players play the same animal type, the player with the higher quantity wins."
    , "   - If both the type and quantity are the same, the round is a draw."
    , "4. Exception: Worms (Rank 5) can defeat Dinosaurs (Rank 1)!"
    , "5. Buffing your deck: Once during the game, the player can choose to \"buff\" their deck. This will evolve all their animals to the next higher rank. For example: "
    , "   - Worms become Chickens, Chickens become Foxes, and so on."
    , "   - Exception: Dinosaurs will evolve into Worms! (LOL)"
    , "6. After each round:"
    , "   - The player who wins the battle gets one point. If it's a draw, no points are awarded."
    , "   - All cards played are discarded."
    , "   - The winner draws a random card to add to their deck."
    , "   - If it's a draw, none of the players get to draw a new card."
    , "7. The game ends when one player has no cards left in their deck. The other player is declared the winner!"
    , "8. Use your strategy to manage your deck wisely and outsmart your opponent!"
    ]

displayCredits :: IO ()
displayCredits = putStrLn $ unlines
    [ "\n==================================="
    , "           Game Credits           "
    , "==================================="
    , "Game Title: Animal Clash"
    , "Developed by: Kong Yao"
    , "Special Thanks to:"
    , " - Dr. Chin Teck Min for guidance"
    , " - OpenAI ChatGPT for assistance"
    , " - Family and friends for support"
    , " - Haskell Community for resources"
    , "==================================="
    , "Thank you for playing Animal Clash!"
    , "==================================="
    ]

main :: IO ()
main = do
    putStrLn "\nWelcome to Animal Clash!" 
    putStrLn "Prepare for an epic card battle where strategy and instincts collide. Will you rise as the ultimate champion?\n" 
    mainMenu

