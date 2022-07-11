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

data TurnResult =
  DealerWon | PlayerWon | War
  deriving (Show, Eq, Enum)

data Card =
  Ace | Two | Three | Four | 
  Five | Six | Seven | Eight | 
  Nine | Ten | Jack | Queen | King
  deriving (Show, Eq, Enum, Ord)

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

cardsHolding :: Player -> [Card]
cardsHolding (Player {name = a, cards = b} ) = b

playerFromGame :: Game -> Player
playerFromGame (Game {dealer = a, player = b} ) = b

dealerFromGame :: Game -> Player
dealerFromGame (Game {dealer = a, player = b} ) = a

dealerHasSameCard :: Card -> Card -> Bool
dealerHasSameCard dealerCard playerCard
    | dealerCard == playerCard = True
    | otherwise = False

dealerHasHigherCard :: Card -> Card -> Bool
dealerHasHigherCard dealerCard playerCard
    | dealerCard > playerCard = True
    | otherwise = False

evalTurnWinner :: Player -> Player -> TurnResult
evalTurnWinner dealer player = do
    let dealerCards = cardsHolding dealer
    let playerCards = cardsHolding player

    let dealerCard = head dealerCards
    let playerCard = head playerCards

    if dealerHasHigherCard dealerCard playerCard
        then do
          DealerWon
            -- Dealer wins this round
        else
            -- Either Player won or it's War
            if dealerHasSameCard dealerCard playerCard
                then do
                  War
                    -- War!
                else do
                  PlayerWon
                    -- Player won this round

-- turn :: Game -> Player 
turn :: Game -> IO ()
turn game
    | (length (cardsHolding (dealerFromGame game))) == 0 = putStrLn "Dealer Lost!"
    | (length (cardsHolding (playerFromGame game))) == 0 = putStrLn "You Lost!"
    | otherwise = do 
        -- putStrLn "Otherwise called"
        let turnResult = evalTurnWinner (dealerFromGame game) (playerFromGame game)
        --if turnResult == Dealer then do
        --    putStrLn "Dealer won that round!"
        --    else
        --      if turnResult == Player then do
        --          putStrLn "Player won that round!"
        --        else
        --            if turnResult == War then do
        --               putStrLn "Cards match, THIS IS WAR!"
        turn game

main :: IO ()
main = do

    -- Set up Game

    putStrLn "Hello, what is your name?: "
    playerName <- getLine
    putStrLn $ "Hello " ++ playerName ++ ", Let's play War!"

    

    -- Create Player instances
    let dealer = Player {name = "Dealer", cards = [] }
    let me = Player {name = playerName, cards = [] }

    -- Deal Cards to each player

    let game = initGame dealer me

    

    -- Play Game
    turn game
    --putStrLn turn.name ++ "won this round"

    putStrLn "End"