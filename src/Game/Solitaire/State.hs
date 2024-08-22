-- | types and functions for the solitaire (also know as Klondike game)
module Game.Solitaire.State
  ( Solitaire(..)
  , Foundations(..)
  , Tableau(..)
  , BuildPile
  , SuitPile
  , Waste
  , Stock
  , setupSolitaire
  , setupFoundations
  ) where

import Game.Components.State

-- Synonyms for different list of Cards
type Waste = [Card]

type SuitPile = [Card]

-- Regular piles can only grow with alternating color
type BuildPile = [Card]

type Stock = [Card]

-- There are four foundations, one for each suit
data Foundations = Foundations
  { heartsPile :: SuitPile,
  diamondsPile :: SuitPile,
  clubsPile :: SuitPile,
  spadesPile :: SuitPile
} deriving (Show)


data Tableau = Tableau {
  one   :: BuildPile,
  two   :: BuildPile,
  three :: BuildPile,
  four  :: BuildPile,
  five  :: BuildPile,
  six   :: BuildPile,
  seven :: BuildPile
} deriving (Show)

-- There are seven Build piles in the Tableau
-- Each pile is labeled 1 through 7
-- type Tableau = [(Int, BuildPile)]

-- The full game state
data Solitaire = Solitaire
  { stock :: Stock
  , waste :: Waste
  , foundations :: Foundations
  , tableau :: Tableau
  }

setupFoundations :: Foundations
setupFoundations = Foundations hp dp cp sp
  where
    hp = [Card EmptyRank Hearts]
    dp = [Card EmptyRank Diamonds]
    cp = [Card EmptyRank Clubs]
    sp = [Card EmptyRank Spades]

-- Deal cards into tableaus
-- output is the list of tableaus and stock
dealTableaus :: Deck -> Int -> Int -> [BuildPile] -> ([BuildPile], Stock)
dealTableaus deck _ 0 acc = (reverse acc, deck)
dealTableaus deck n count acc =
  dealTableaus rest (n + 1) (count - 1) (newPile : acc)
  where
    (dealt, rest) = splitAt n deck
    newPile = dealt

setupTableau :: [BuildPile] -> Tableau
setupTableau bps = Tableau p1 p2 p3 p4 p5 p6 p7
  where
    p1:rest1 = bps
    p2:rest2 = rest1
    p3:rest3 = rest2
    p4:rest4 = rest3
    p5:rest5 = rest4
    p6:rest6 = rest5
    p7 = head rest6

-- Setup Solitaire game given a deck in a certain state
-- The game starts with
-- - A stock with the left over cards
-- - an empty waste
-- - seven piles in the tableau
-- - four empty foundation piles
setupSolitaire :: Deck -> Solitaire
setupSolitaire shuffledDeck = Solitaire rest [] initFoundations initTableau
  where
    (tableaus, rest) = dealTableaus shuffledDeck 1 7 []
    initFoundations = setupFoundations
    initTableau = setupTableau tableaus
