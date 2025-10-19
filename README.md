# Animal Clash 🐾

A Haskell-based console card game where players compete using decks of animal cards, demonstrating functional programming concepts such as monadic structures, polymorphism, custom data types, and typeclasses.

## Features

- **Animal Card System**: Five different animal types (Worm, Chicken, Fox, Bear, Dinosaur) with unique ranks and quantities
- **Strategic Battle Mechanism**: Winner determination based on animal ranks, quantities, and special rules
- **Dynamic Deck Management**: Card addition, removal, and upgrade functionality
- **Interactive Game Loop**: Turn-based gameplay with score tracking and state management
- **Deck Buffing System**: One-time upgrade option to enhance all cards in deck
- **Intuitive Menu System**: User-friendly interface with clear prompts and input validation
- **Extensible Design**: Built with functional programming principles for flexibility and scalability

## Tech Stack

- **Language**: Haskell
- **Compiler**: GHC (Glasgow Haskell Compiler)
- **Core Concepts Applied**:
  - Custom data types and typeclasses
  - Monadic structures (IO, Maybe, Custom Card monad)
  - Higher-order functions and function composition
  - Pattern matching and recursion
  - Pure functions and immutable data structures
  - Functor, Applicative, and Monad instances

## How to Run

1. **Prerequisites**: Ensure you have GHC (Glasgow Haskell Compiler) installed
   ```bash
   # Check if GHC is installed
   ghc --version
   ```

2. **Clone the repository**:
   ```bash
   git clone https://github.com/kongyao19/animal-clash-haskell.git
   cd animal-clash-haskell
   ```

### Method 1: Compile and Run
3. **Compile the program**:
   ```bash
   ghc -o animal_clash Main.hs
   ```

4. **Run the game**:
   ```bash
   ./animal_clash
   ```

### Method 2: Run with GHCi (Interactive)
3. **Start GHCi and load the main file**:
   ```bash
   ghci Main.hs
   ```

4. **Run the main function**:
   ```haskell
   *Main> main
   ```

5. **To exit GHCi when done**:
   ```haskell
   *Main> :quit
   ```

## Game Rules

### Deck Composition
- Each player starts with 15 cards total:
  - **Dinosaur** (Rank 1 - Strongest): 1 card
  - **Bear** (Rank 2): 2 cards  
  - **Fox** (Rank 3): 3 cards
  - **Chicken** (Rank 4): 4 cards
  - **Worm** (Rank 5 - Weakest): 5 cards

### Gameplay
- Players take turns selecting animal type and quantity to play
- **Battle Resolution**:
  - Higher-ranked animals defeat lower-ranked animals
  - Same animal type: higher quantity wins
  - Same type and quantity: draw
  - **Special Rule**: Worms can defeat Dinosaurs!

### Deck Buffing
- Once per game, players can "buff" their entire deck
- All animals evolve to next higher rank
- Exception: Dinosaurs evolve into Worms

### Winning Conditions
- Round winner gains 1 point and draws a random card
- Cards played each round are discarded
- Game ends when a player runs out of cards
- Player with remaining cards wins

## Learning Outcomes

This project demonstrates practical application of functional programming concepts in Haskell:

- **Data Modeling**: Creating custom data types (`Animal`, `Card`, `GameState`) with appropriate typeclass instances
- **Monadic Programming**: Implementing custom Monad instance for `Card` type and effective use of IO monad
- **Pure Functional Design**: Separating pure game logic from side effects, ensuring predictable behavior
- **Higher-Order Functions**: Utilizing `map`, `fmap`, and function composition for data transformations
- **Pattern Matching**: Comprehensive use of pattern matching for game logic and input validation
- **Modular Architecture**: Building maintainable, reusable components with clear separation of concerns

The challenges faced during development, particularly with game loop organization and time management, provided valuable experience in software design and project planning.

## Credits

**Developer**: Kong Yao  
**Institution**: Sunway University, School of Engineering and Technology, BSc (Hons) Computer Science  
**Course**: PRG2214 - Functional Programming Principles  
**Academic Session**: September 2024

**Special Thanks**:
- Dr. Chin Teck Min for guidance
- Haskell Community for resources and documentation

---

*This project was developed as part of the Functional Programming Principles course, showcasing the practical application of Haskell in game development.*
