module Main where

import System.Random
import Control.Monad
import Control.Monad.Random
--import Control.Monad.State
import Control.Monad.IO.Class
--import Data.List (sort, intercalate)

import qualified System.Random as Rand
import System.Random.Shuffle (shuffleM)

data TurnResult =
  DealerWon | PlayerWon | War
  deriving (Show, Eq, Enum)

data Card =
  Ace | Two | Three | Four | 
  Five | Six | Seven | Eight | 
  Nine | Ten | Jack | Queen | King
  deriving (Show, Eq, Enum, Ord)

type Deck = [Card]

fullDeck :: Deck
fullDeck = concat $ replicate 4 fullSuit
  where
    fullSuit = [ Ace, Two, Three, Four, 
                 Five, Six, Seven, Eight, 
                 Nine, Ten, Jack, Queen, King
               ]

-- Shuffled Deck
shuffledDeck :: Rand.StdGen -> ([Card], Rand.StdGen)
shuffledDeck gen = runRand (shuffleM fullDeck) gen

cardValue :: Card -> Word
cardValue Ace = 1
cardValue Two = 2
cardValue Three = 3
cardValue Four = 4
cardValue Five = 5
cardValue Six = 6
cardValue Seven = 7
cardValue Eight = 8
cardValue Nine = 9
cardValue Ten = 10
cardValue Jack = 10
cardValue Queen = 10
cardValue King = 10

cardToString :: Card -> String
cardToString Ace = "A"
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


--shuffleCards :: Deck -> Deck -> IO Deck
--shuffleCards shuffled [] = return shuffled
--shuffleCards shuffled unshuffled = do
--  randomCardIndex <- randomRIO (0, length unshuffled - 1)
--  let randomCard = unshuffled !! randomCardIndex
--      unshuffledBefore = take randomCardIndex unshuffled
--      unshuffledAfter = drop (randomCardIndex + 1) unshuffled
--  
--  shuffleCards (randomCard:shuffled) (unshuffledBefore ++ unshuffledAfter)

--shuffleDeck :: IO Deck
--shuffleDeck = shuffleCards [] fullDeck


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
    gen <- getStdGen

    putStrLn "Hello, what is your name?: "
    playerName <- getLine
    putStrLn $ "Hello " ++ playerName ++ ", Let's play War!"

    -- Shuffle Deck
    --let shuffledDeck = shuffleDeck
    let (newDeck, newGen) = shuffledDeck gen
    let splitDeckIn2 = splitAt 26 newDeck
    let dealerCards = fst splitDeckIn2
    let playerCards = snd splitDeckIn2

    -- Create Player instances
    let dealer = Player {name = "Dealer", cards = dealerCards }
    let me = Player {name = playerName, cards = playerCards }

    -- Deal Cards to each player

    let game = initGame dealer me

    

    -- Play Game
    turn game
    --putStrLn turn.name ++ "won this round"

    putStrLn "End"