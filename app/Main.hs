module Main (main) where

import Game.Components.State
import Game.Solitaire.State

main :: IO ()
main = do
    shuffledDeck <- shuffleDeck createDeck
    let soli  = setupSolitaire shuffledDeck
    putStrLn "\nStock:"
    print $ stock soli
