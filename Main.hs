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

data Player         = Player {
    name            :: String,
    numCardsAquired :: Int,
    cards           :: [Card]
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

--cardsHolding :: Player -> Int
--cardsHolding player = do
--    let cardsRemaining = player{numCardsAquired}
--    return cardsRemaining

cardsHolding :: Player -> Int
cardsHolding (Player {name = a, numCardsAquired = b, cards = c} ) = b

playerFromGame :: Game -> Player
playerFromGame (Game {dealer = a, player = b} ) = b

dealerFromGame :: Game -> Player
dealerFromGame (Game {dealer = a, player = b} ) = a

evalTurnWinner :: Player -> Player -> Player
evalTurnWinner dealer player = do
    dealer

-- turn :: Game -> Player 
turn :: Game -> IO ()
turn game
    | (cardsHolding (dealerFromGame game)) == 0 = putStrLn "Dealer Lost!"
    | (cardsHolding (playerFromGame game)) == 0 = putStrLn "You Lost!"
        --return dealerFromGame game
    -- | game.dealer.numCardsAquired == 0 = putStrLn (game.dealer.name ++ "lost! ")
    -- | game.player.numCardsAquired == 0 = putStrLn (game.player.name ++ "lost! ")
    | otherwise = do 
        -- putStrLn "Otherwise called"
        --let turnWinner = evalTurnWinner (dealerFromGame game) (playerFromGame game)
        --putStrLn turnWinner.name ++ "won this round"
        -- return turnWinner
        putStrLn "OTHERWISE CASE WON"


main :: IO ()
main = do

    -- Set up Game

    putStrLn "Hello, what is your name?: "
    playerName <- getLine
    putStrLn $ "Hello " ++ playerName ++ ", Let's play War!"

    let dealer = Player {name = "Dealer", numCardsAquired = 1, cards = [] }
    let me = Player {name = playerName, numCardsAquired = 0, cards = [] }

    let game = initGame dealer me

    -- Play Game
    turn game
    --putStrLn turn.name ++ "won this round"

    putStrLn "End"