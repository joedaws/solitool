-- | Module for defining data types for Cards

module Game.Components.State

  ( Card(..)
  , Suit(..)
  , Rank(..)
  , Deck
  , Color
  , color
  , createDeck
  , shuffleDeck
  ) where

import Data.List (unfoldr)
import System.Random

data Suit
  = Hearts
  | Diamonds
  | Clubs
  | Spades
  deriving (Show, Enum, Bounded, Eq)

data Rank
  = EmptyRank
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving (Show, Enum, Bounded, Eq, Ord)

-- Define a card as a combination of a rank and a suit
data Card = Card
  { rank :: Rank
  , suit :: Suit
  } deriving (Show, Eq)

data Color = Red | Black deriving (Show, Eq)

color :: Card -> Color
color (Card _ Hearts) = Red
color (Card _ Diamonds) = Red
color (Card _ Clubs) = Black
color (Card _ Spades) = Black

type Deck = [Card]

-- Generate a standard deck of 52 cards in the cannonical order
createDeck :: Deck
createDeck =
  [ Card r s
  | r <- [Ace .. maxBound]
  , s <- [minBound .. maxBound]
  ]

-- Shuffle the deck
shuffleDeck :: Deck -> IO Deck
shuffleDeck d = do
  shuffle' d (length d) <$> newStdGen

-- Fisher-Yates shuffle algorithm
shuffle' :: [a] -> Int -> StdGen -> [a]
shuffle' xs n gen = unfoldr select (xs, n, gen)
  where
    select ([], _, _) = Nothing
    select (ys, k, g) =
      let (i, g') = randomR (0, k - 1) g
          (front, (a:back)) = splitAt i ys
       in Just (a, (front ++ back, k - 1, g'))
