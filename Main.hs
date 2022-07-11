{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.Random
import Control.Monad
import Control.Monad.Random
import Control.Monad.IO.Class
import Language.Haskell.Exts

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

data Player         = Player {
    username        :: String,
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

cardsHolding :: Player -> [Card]
cardsHolding (Player {username = a, cards = b} ) = b

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
        -- The Draw
        let dealerCards = (cardsHolding (dealerFromGame game))
        let playerCards = (cardsHolding (playerFromGame game))
        let dealerCard = head dealerCards
        let playerCard = head playerCards

        putStrLn "Dealer Draws a: "
        putStrLn (cardToString dealerCard)

        putStrLn "You Draw a: "
        putStrLn (cardToString playerCard)

        let turnResult = evalTurnWinner (dealerFromGame game) (playerFromGame game)
        if turnResult == DealerWon 
          then do 
            putStrLn "Dealer won that round!"
            -- Dealer takes the cards

            let dealerCardsAfterWin = ((tail dealerCards) ++ [(head playerCards)] ++ [(head dealerCards)])
            let playerCardsAfterLoss = tail playerCards

            let dealer = (dealerFromGame game)
            let newDealer = dealer{cards = dealerCardsAfterWin}

            let me = (playerFromGame game)
            let newMe = me{cards = playerCardsAfterLoss}
            
            let newGame = initGame newDealer newMe

            putStrLn "Here is the new game state:\n"
            print ("Dealer now has " ++ showInt (length (cardsHolding (dealerFromGame newGame))) ++ " cards")
            print ("You now have " ++ showInt (length (cardsHolding (playerFromGame newGame))) ++ " cards")
            putStrLn "\n\n"
            turn newGame
          else
            if turnResult == PlayerWon 
              then do
                putStrLn "Player won that round!"
                -- Player takes the cards

                let dealerCardsAfterLoss = tail dealerCards
                let playerCardsAfterWin = ((tail playerCards) ++ [(head playerCards)] ++ [(head dealerCards)])

                let dealer = (dealerFromGame game)
                let newDealer = dealer{cards = dealerCardsAfterLoss}

                let me = (playerFromGame game)
                let newMe = me{cards = playerCardsAfterWin}
            
                let newGame = initGame newDealer newMe

                putStrLn "Here is the new game state:\n"
                print ("Dealer now has " ++ showInt (length (cardsHolding (dealerFromGame newGame))) ++ " cards")
                print ("You now have " ++ showInt (length (cardsHolding (playerFromGame newGame))) ++ " cards")
                putStrLn "\n\n"
                turn newGame
              else
            if turnResult == War 
              then do
                putStrLn "Cards match, THIS IS WAR!"
                -- TODO: Recursively call turn logic with a temporary store for cards drawn, winner takes all cards from the War
                --turn game
              else
                putStrLn "TURN RESULT == WAR ELSE"
        --turn game
      
main :: IO ()
main = do

    -- Set up Game
    gen <- getStdGen

    putStrLn "Hello, what is your name?: "
    playerName <- getLine
    putStrLn $ "Hello " ++ playerName ++ ", Let's play War!"

    -- Shuffle Deck
    let (newDeck, newGen) = shuffledDeck gen
    let splitDeckIn2 = splitAt 26 newDeck
    let dealerCards = fst splitDeckIn2
    let playerCards = snd splitDeckIn2

    -- Create Player instances
    let dealer = Player {username = "Dealer", cards = dealerCards }
    let me = Player {username = playerName, cards = playerCards }

    -- Deal Cards to each player
    let game = initGame dealer me

    -- Play Game
    turn game
    putStrLn "End"