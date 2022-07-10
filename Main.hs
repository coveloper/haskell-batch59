module Main where

import System.Random
import Control.Monad
--import Control.Monad.Random
--import Control.Monad.State
import Control.Monad.IO.Class
--import Data.List (sort, intercalate)

import qualified System.Random as Rand
--import System.Random.Shuffle (shuffleM)

-- My Imports
--import Data

-- Shuffled Deck
--shuffledDeck :: Rand.StdGen -> ([Card], Rand.StdGen)
--shuffledDeck gen = runRand (shuffleM baseDeck) gen

data Card =
  Ace | Two | Three | Four | 
  Five | Six | Seven | Eight | 
  Nine | Ten | Jack | Queen | King
  deriving (Show, Eq, Enum)

baseDeck :: [Card]
baseDeck = concat $ replicate 4 fullSuit
  where
    fullSuit = [ Ace, Two, Three, Four, 
                 Five, Six, Seven, Eight, 
                 Nine, Ten, Jack, Queen, King
               ]

cardScore :: Card -> Word
cardScore Two = 2
cardScore Three = 3
cardScore Four = 4
cardScore Five = 5
cardScore Six = 6
cardScore Seven = 7
cardScore Eight = 8
cardScore Nine = 9
cardScore Ten = 10
cardScore Jack = 10
cardScore Queen = 10
cardScore King = 10
cardScore Ace = 1

cardToString :: Card -> String
cardToString Two = "2"
cardToString Three = "3"
cardToString Four = "4"
cardToString Five = "5"
cardToString Six = "6"
cardToString Seven = "7"
cardToString Eight = "8"
cardToString Nine = "9"
cardToString Ten = "10"
cardToString Jack = "J"
cardToString Queen = "Q"
cardToString King = "K"
cardToString Ace = "A"

--data WarAction = Deal
--  deriving (Show, Eq, Enum)

--data WarObservation = WarObservation
--  { playerScore :: Word
--  , playerHasAce :: Bool
--  , dealerCardShowing :: Card
--  } deriving (Show)

--data WarEnvironment = WarEnvironment
--  { currentObservation :: WarObservation
--  , dealerHand :: (Card, Card, [Card]) -- Shown card, hidden card, dealt cards
--  , playerHand :: [Card]
--  , deck :: [Card]
--  , randomGenerator :: Rand.StdGen
--  , playerHasStood :: Bool
--  } deriving (Show)

data Player         = Player {
    name            :: String,
    numCardsAquired :: Int
  } deriving Show

data Game       = Game {
    dealer     :: Player,
    player     :: Player
  } deriving Show

initGame :: Player -> Player -> Game
initGame player1 player2 = Game {
       dealer = player1,
       player = player2
     }


--initGame :: MonadIO m => m Game
--initGame = do
--  let game      = Game {
--    player     :: Player,
--    dealer     :: Player
--  }
--  return $ game


main :: IO ()
main = do
    putStrLn "Hello, what is your name?: "
    playerName <- getLine
    putStrLn $ "Hello " ++ playerName ++ ", Let's play War!"

    let dealer = Player {name = "Dealer", numCardsAquired = 0 }
    let me = Player {name = playerName, numCardsAquired = 0 }

    let game = initGame dealer me

    putStrLn "End"