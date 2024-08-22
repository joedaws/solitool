import Test.Hspec
import Game.Components.State
import Game.Solitaire.State
import Game.Solitaire.Transitions

main :: IO ()
main = hspec $ do
  describe "Game setup" $ do
    it "setupSolitaire" $ do
      let deck = createDeck
      let s = setupSolitaire deck
      let initStock = stock s
      let initFoundations = foundations s
      length initStock `shouldBe` 24
      head initStock `shouldBe` Card Eight Hearts
      heartsPile initFoundations `shouldBe` [Card EmptyRank Hearts]
      waste s `shouldBe` []
      let oneTableau = one $ tableau s
      oneTableau `shouldBe` [Card Ace Hearts]

  describe "Solitaire State Transitions" $ do
    it "stockToWaste when stock has cards" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [] []
      let s = Solitaire [Card Two Hearts] [] initFoundations initTableau
      let s' = stockToWaste s
      stock s' `shouldBe` []
      waste s' `shouldBe` [Card Two Hearts]

    it "stockToWaste when no cards in stock" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [Card Three Clubs] [] [] [] [] [] []
      let s = Solitaire [] [Card Two Hearts] initFoundations initTableau
      let s' = stockToWaste s
      stock s' `shouldBe` []
      waste s' `shouldBe` [Card Two Hearts]

    it "refreshStock when stock is empty" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [] []
      let s = Solitaire [] [Card Two Hearts, Card Three Hearts] initFoundations initTableau
      let s' = refreshStock s
      stock s' `shouldBe` [Card Three Hearts, Card Two Hearts]
      waste s' `shouldBe` []

    it "refreshStock when stock is non-empty" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [] []
      let s = Solitaire [Card Ace Hearts] [Card Two Hearts] initFoundations initTableau
      let s' = refreshStock s
      stock s' `shouldBe` [Card Ace Hearts]
      waste s' `shouldBe` [Card Two Hearts]

    it "wasteToTableauOne when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [Card Three Clubs] [] [] [] [] [] []
      let s = Solitaire [] [Card Two Hearts] initFoundations initTableau
      let s' = wasteToTableauOne s
      let t' = one $ tableau s'
      t' `shouldBe` [Card Two Hearts, Card Three Clubs]

    it "wasteToTableauOne when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [Card Three Clubs] [] [] [] [] [] []
      let s = Solitaire [] [Card Five Hearts] initFoundations initTableau
      let s' = wasteToTableauOne s
      let t' = one $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    it "wasteToTableauTwo when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [Card Three Clubs] [Card King Spades] [] [] [] [] []
      let s = Solitaire [] [Card Queen Hearts] initFoundations initTableau
      let s' = wasteToTableauTwo s
      let t' = two $ tableau s'
      t' `shouldBe` [Card Queen Hearts, Card King Spades]

    it "wasteToTableauTwo when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [Card Three Clubs] [] [] [] [] []
      let s = Solitaire [] [Card Five Hearts] initFoundations initTableau
      let s' = wasteToTableauTwo s
      let t' = two $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    it "wasteToTableauThree when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [Card Three Clubs] [Card King Spades] [] [] [] []
      let s = Solitaire [] [Card Queen Hearts] initFoundations initTableau
      let s' = wasteToTableauThree s
      let t' = three $ tableau s'
      t' `shouldBe` [Card Queen Hearts, Card King Spades]

    it "wasteToTableauThree when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [Card Three Clubs]  [] [] [] []
      let s = Solitaire [] [Card Five Hearts] initFoundations initTableau
      let s' = wasteToTableauThree s
      let t' = three $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    it "wasteToTableauFour when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [Card Two Spades]  [] [] []
      let s = Solitaire [] [Card Ace Hearts] initFoundations initTableau
      let s' = wasteToTableauFour s
      let t' = four $ tableau s'
      t' `shouldBe` [Card Ace Hearts, Card Two Spades]

    it "wasteToTableauFour when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [Card Three Clubs] [] [] []
      let s = Solitaire [] [Card Five Spades] initFoundations initTableau
      let s' = wasteToTableauFour s
      let t' = four $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    it "wasteToTableauFive when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [Card Two Spades] [] []
      let s = Solitaire [] [Card Ace Hearts] initFoundations initTableau
      let s' = wasteToTableauFive s
      let t' = five $ tableau s'
      t' `shouldBe` [Card Ace Hearts, Card Two Spades]

    it "wasteToTableauFive when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [Card Three Clubs] [] []
      let s = Solitaire [] [Card Five Spades] initFoundations initTableau
      let s' = wasteToTableauFive s
      let t' = five $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    it "wasteToTableauSix when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [Card Two Spades] []
      let s = Solitaire [] [Card Ace Hearts] initFoundations initTableau
      let s' = wasteToTableauSix s
      let t' = six $ tableau s'
      t' `shouldBe` [Card Ace Hearts, Card Two Spades]

    it "wasteToTableauSix when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [Card Three Clubs] []
      let s = Solitaire [] [Card Five Spades] initFoundations initTableau
      let s' = wasteToTableauSix s
      let t' = six $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    it "wasteToTableauSeven when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [] [Card Two Spades]
      let s = Solitaire [] [Card Ace Hearts] initFoundations initTableau
      let s' = wasteToTableauSeven s
      let t' = seven $ tableau s'
      t' `shouldBe` [Card Ace Hearts, Card Two Spades]

    it "wasteToTableauSeven when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [] [Card Three Clubs]
      let s = Solitaire [] [Card Five Spades] initFoundations initTableau
      let s' = wasteToTableauSeven s
      let t' = seven $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    it "tableauToTableau when toBuildPile can be built" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [Card Three Clubs] [Card Two Hearts] [] [] [] [] []
      let s = Solitaire [] [] initFoundations initTableau
      let s' = tableauToTableau 2 1 1 s
      let t1' = one $ tableau s'
      t1' `shouldBe` [Card Two Hearts, Card Three Clubs]

    it "tableauToTableau when toBuildPile can be built move 2" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [Card Five Clubs] [Card Three Spades, Card Four Hearts] [] [] [] [] []
      let s = Solitaire [] [] initFoundations initTableau
      let s' = tableauToTableau 2 1 2 s
      let t1' = one $ tableau s'
      t1' `shouldBe` [Card Three Spades, Card Four Hearts, Card Five Clubs]
