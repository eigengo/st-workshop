module Workshop.RandomnessSpec(spec) where
  import Workshop.Randomness
  import Test.Hspec

  spec :: Spec
  spec = do
    -- Big data my ar*e
    describe "'Big data' support" $ do
      -- sum of all age fields in the list
      it "computes total age" $ do
        total [personOfAge 10] `shouldBe` 10
        total [personOfAge x | x <- [0..20]] `shouldBe` 210
      -- average age
      it "computes average age" $ do
        average [personOfAge x | x <- [0..20]] `shouldBe` 10
      -- partition into young and old
      it "partitions people" $ do
        let (old, young) = ages [personOfAge x | x <- [40..60]]
        length old `shouldBe` 10
        length young `shouldBe` 11

    -- the random Person generator
    describe "Random generator" $ do
      -- the ages are 0..100, over enough records, the average should be ~50
      it "generates good distribution of ages" $ do
        ps <- people
        average ps `shouldSatisfy` (< 1) . abs . (50 -)

    where
      personOfAge = Person "a" "b" 