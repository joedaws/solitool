-- | functions to represent transitions in state

module Game.Solitaire.Transitions
  ( canBuild
  , stockToWaste
  , refreshStock
  , wasteToTableauOne
  , wasteToTableauTwo
  , wasteToTableauThree
  , wasteToTableauFour
  , wasteToTableauFive
  , wasteToTableauSix
  , wasteToTableauSeven
  , wasteMinusOne
  , tableauToTableau) where

import Game.Components.State
import Game.Solitaire.State

canBuild :: Card -> Card -> Bool
canBuild (Card Ace Hearts) (Card Two Clubs) = True
canBuild (Card Ace Hearts) (Card Two Spades) = True
canBuild (Card Ace Diamonds) (Card Two Clubs) = True
canBuild (Card Ace Diamonds) (Card Two Spades) = True
canBuild (Card Ace Clubs) (Card Two Diamonds) = True
canBuild (Card Ace Clubs) (Card Two Hearts) = True
canBuild (Card Ace Spades) (Card Two Diamonds) = True
canBuild (Card Ace Spades) (Card Two Hearts) = True

canBuild (Card Two Hearts) (Card Three Clubs) = True
canBuild (Card Two Hearts) (Card Three Spades) = True
canBuild (Card Two Diamonds) (Card Three Clubs) = True
canBuild (Card Two Diamonds) (Card Three Spades) = True
canBuild (Card Two Clubs) (Card Three Diamonds) = True
canBuild (Card Two Clubs) (Card Three Hearts) = True
canBuild (Card Two Spades) (Card Three Diamonds) = True
canBuild (Card Two Spades) (Card Three Hearts) = True

canBuild (Card Three Hearts) (Card Four Clubs) = True
canBuild (Card Three Hearts) (Card Four Spades) = True
canBuild (Card Three Diamonds) (Card Four Clubs) = True
canBuild (Card Three Diamonds) (Card Four Spades) = True
canBuild (Card Three Clubs) (Card Four Diamonds) = True
canBuild (Card Three Clubs) (Card Four Hearts) = True
canBuild (Card Three Spades) (Card Four Diamonds) = True
canBuild (Card Three Spades) (Card Four Hearts) = True

canBuild (Card Four Hearts) (Card Five Clubs) = True
canBuild (Card Four Hearts) (Card Five Spades) = True
canBuild (Card Four Diamonds) (Card Five Clubs) = True
canBuild (Card Four Diamonds) (Card Five Spades) = True
canBuild (Card Four Clubs) (Card Five Diamonds) = True
canBuild (Card Four Clubs) (Card Five Hearts) = True
canBuild (Card Four Spades) (Card Five Diamonds) = True
canBuild (Card Four Spades) (Card Five Hearts) = True

canBuild (Card Five Hearts) (Card Six Clubs) = True
canBuild (Card Five Hearts) (Card Six Spades) = True
canBuild (Card Five Diamonds) (Card Six Clubs) = True
canBuild (Card Five Diamonds) (Card Six Spades) = True
canBuild (Card Five Clubs) (Card Six Diamonds) = True
canBuild (Card Five Clubs) (Card Six Hearts) = True
canBuild (Card Five Spades) (Card Six Diamonds) = True
canBuild (Card Five Spades) (Card Six Hearts) = True

canBuild (Card Six Hearts) (Card Seven Clubs) = True
canBuild (Card Six Hearts) (Card Seven Spades) = True
canBuild (Card Six Diamonds) (Card Seven Clubs) = True
canBuild (Card Six Diamonds) (Card Seven Spades) = True
canBuild (Card Six Clubs) (Card Seven Diamonds) = True
canBuild (Card Six Clubs) (Card Seven Hearts) = True
canBuild (Card Six Spades) (Card Seven Diamonds) = True
canBuild (Card Six Spades) (Card Seven Hearts) = True

canBuild (Card Seven Hearts) (Card Eight Clubs) = True
canBuild (Card Seven Hearts) (Card Eight Spades) = True
canBuild (Card Seven Diamonds) (Card Eight Clubs) = True
canBuild (Card Seven Diamonds) (Card Eight Spades) = True
canBuild (Card Seven Clubs) (Card Eight Diamonds) = True
canBuild (Card Seven Clubs) (Card Eight Hearts) = True
canBuild (Card Seven Spades) (Card Eight Diamonds) = True
canBuild (Card Seven Spades) (Card Eight Hearts) = True

canBuild (Card Eight Hearts) (Card Nine Clubs) = True
canBuild (Card Eight Hearts) (Card Nine Spades) = True
canBuild (Card Eight Diamonds) (Card Nine Clubs) = True
canBuild (Card Eight Diamonds) (Card Nine Spades) = True
canBuild (Card Eight Clubs) (Card Nine Diamonds) = True
canBuild (Card Eight Clubs) (Card Nine Hearts) = True
canBuild (Card Eight Spades) (Card Nine Diamonds) = True
canBuild (Card Eight Spades) (Card Nine Hearts) = True

canBuild (Card Nine Hearts) (Card Ten Clubs) = True
canBuild (Card Nine Hearts) (Card Ten Spades) = True
canBuild (Card Nine Diamonds) (Card Ten Clubs) = True
canBuild (Card Nine Diamonds) (Card Ten Spades) = True
canBuild (Card Nine Clubs) (Card Ten Diamonds) = True
canBuild (Card Nine Clubs) (Card Ten Hearts) = True
canBuild (Card Nine Spades) (Card Ten Diamonds) = True
canBuild (Card Nine Spades) (Card Ten Hearts) = True

canBuild (Card Ten Hearts) (Card Jack Clubs) = True
canBuild (Card Ten Hearts) (Card Jack Spades) = True
canBuild (Card Ten Diamonds) (Card Jack Clubs) = True
canBuild (Card Ten Diamonds) (Card Jack Spades) = True
canBuild (Card Ten Clubs) (Card Jack Diamonds) = True
canBuild (Card Ten Clubs) (Card Jack Hearts) = True
canBuild (Card Ten Spades) (Card Jack Diamonds) = True
canBuild (Card Ten Spades) (Card Jack Hearts) = True

canBuild (Card Jack Hearts) (Card Queen Clubs) = True
canBuild (Card Jack Hearts) (Card Queen Spades) = True
canBuild (Card Jack Diamonds) (Card Queen Clubs) = True
canBuild (Card Jack Diamonds) (Card Queen Spades) = True
canBuild (Card Jack Clubs) (Card Queen Diamonds) = True
canBuild (Card Jack Clubs) (Card Queen Hearts) = True
canBuild (Card Jack Spades) (Card Queen Diamonds) = True
canBuild (Card Jack Spades) (Card Queen Hearts) = True

canBuild (Card Queen Hearts) (Card King Clubs) = True
canBuild (Card Queen Hearts) (Card King Spades) = True
canBuild (Card Queen Diamonds) (Card King Clubs) = True
canBuild (Card Queen Diamonds) (Card King Spades) = True
canBuild (Card Queen Clubs) (Card King Diamonds) = True
canBuild (Card Queen Clubs) (Card King Hearts) = True
canBuild (Card Queen Spades) (Card King Diamonds) = True
canBuild (Card Queen Spades) (Card King Hearts) = True

canBuild _ _ = False

-- Transition state functions

{- | Move the top card in the stock to the waste

When the stock is empty this moves the waste cards
back into the stock.
-}
stockToWaste :: Solitaire -> Solitaire
stockToWaste s
  | stock s /= [] = stockToWaste' s
  | otherwise = s

-- | Helper transition function.
--   Moves the top stock card to the top of the waste
--   Warning cannot be used with stock is empty
stockToWaste' :: Solitaire -> Solitaire
stockToWaste' s = newSolitaire
  where
    c:cs = stock s
    newStock = cs
    newWaste =  c:waste s
    newSolitaire = s {stock = newStock, waste = newWaste}

-- | Helper transition function.
--   Moves the cards in the waste to the stock and emtpies waste
refreshStock :: Solitaire -> Solitaire
refreshStock s
  | null $ stock s = s {stock = reverse $ waste s, waste = []}
  | otherwise = s



wasteToTableauOne :: Solitaire -> Solitaire
wasteToTableauOne s
  | null $ waste s = s
  | null firstTableau = s
  | canBuild (head $ waste s) (head firstTableau) = s {waste = wasteMinusOne (waste s), tableau = initTableau {one = (head $ waste s):firstTableau}}
  | otherwise = s
  where
    initTableau = tableau s
    firstTableau = one initTableau

wasteToTableauTwo :: Solitaire -> Solitaire
wasteToTableauTwo s
  | null $ waste s = s
  | null secondTableau = s
  | canBuild (head $ waste s) (head secondTableau) = s {waste = wasteMinusOne (waste s), tableau = initTableau {two = (head $ waste s):secondTableau}}
  | otherwise = s
  where
    initTableau = tableau s
    secondTableau = two initTableau

wasteToTableauThree :: Solitaire -> Solitaire
wasteToTableauThree s
  | null $ waste s = s
  | null newTableau = s
  | canBuild (head $ waste s) (head newTableau) = s {waste = wasteMinusOne (waste s), tableau = initTableau {three = (head $ waste s):newTableau}}
  | otherwise = s
  where
    initTableau = tableau s
    newTableau = three initTableau

wasteToTableauFour :: Solitaire -> Solitaire
wasteToTableauFour s
  | null $ waste s = s
  | null newTableau = s
  | canBuild (head $ waste s) (head newTableau) = s {waste = wasteMinusOne (waste s), tableau = initTableau {four = (head $ waste s):newTableau}}
  | otherwise = s
  where
    initTableau = tableau s
    newTableau = four initTableau

wasteToTableauFive :: Solitaire -> Solitaire
wasteToTableauFive s
  | null $ waste s = s
  | null newTableau = s
  | canBuild (head $ waste s) (head newTableau) = s {waste = wasteMinusOne (waste s), tableau = initTableau {five = (head $ waste s):newTableau}}
  | otherwise = s
  where
    initTableau = tableau s
    newTableau = five initTableau

wasteToTableauSix :: Solitaire -> Solitaire
wasteToTableauSix s
  | null $ waste s = s
  | null newTableau = s
  | canBuild (head $ waste s) (head newTableau) = s {waste = wasteMinusOne (waste s), tableau = initTableau {six = (head $ waste s):newTableau}}
  | otherwise = s
  where
    initTableau = tableau s
    newTableau = six initTableau

wasteToTableauSeven :: Solitaire -> Solitaire
wasteToTableauSeven s
  | null $ waste s = s
  | null newTableau = s
  | canBuild (head $ waste s) (head newTableau) = s {waste = wasteMinusOne (waste s), tableau = initTableau {seven = (head $ waste s):newTableau}}
  | otherwise = s
  where
    initTableau = tableau s
    newTableau = seven initTableau

wasteMinusOne :: Waste -> Waste
wasteMinusOne (_:ws) = ws

tableauToTableau :: Int -> Int -> Int -> Solitaire -> Solitaire
tableauToTableau fromIdx toIdx numCards s
  | canBuild fromCard toCard = s {tableau = updatedTableau}
  | otherwise = s
  where
    t = tableau s
    fromBuildPile = getBuildPile fromIdx t
    toBuildPile = getBuildPile toIdx t
    (cardsToMove, newFromBuildPile) = splitAt numCards fromBuildPile
    fromCard = last cardsToMove
    toCard = head toBuildPile
    updatedToPile = cardsToMove ++ toBuildPile
    updatedTableau = updateTableau fromIdx newFromBuildPile toIdx updatedToPile t

getBuildPile :: Int -> Tableau -> BuildPile
getBuildPile idx tableau' = case idx of
  1 -> one tableau'
  2 -> two tableau'
  3 -> three tableau'
  4 -> four tableau'
  5 -> five tableau'
  6 -> six tableau'
  7 -> seven tableau'
  _ -> error "Invalid tableau index"

updateTableau :: Int -> BuildPile -> Int -> BuildPile -> Tableau -> Tableau
updateTableau fromIdx newFromBuildPile toIdx newToBuildPile t = t {
    one = if fromIdx == 1 then newFromBuildPile else if toIdx == 1 then newToBuildPile else one t,
    two = if fromIdx == 2 then newFromBuildPile else if toIdx == 2 then newToBuildPile else two t,
    three = if fromIdx == 3 then newFromBuildPile else if toIdx == 3 then newToBuildPile else three t,
    four = if fromIdx == 4 then newFromBuildPile else if toIdx == 4 then newToBuildPile else four t,
    five = if fromIdx == 5 then newFromBuildPile else if toIdx == 5 then newToBuildPile else five t,
    six = if fromIdx == 6 then newFromBuildPile else if toIdx == 6 then newToBuildPile else six t,
    seven = if fromIdx == 7 then newFromBuildPile else if toIdx == 7 then newToBuildPile else seven t
  }
