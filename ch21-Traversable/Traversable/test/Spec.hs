module Main (main) where

import Traversable
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = hspec $ do
  describe "specs" $ do
    describe "Identity" $
      it "traverse" $
        traverse (: []) (Identity 3) `shouldBe` [Identity 3]
    describe "List" $
      let onlyThree x = if x == 3 then Just x else Nothing
      in do
        it "traverse" $
          traverse onlyThree (Cons 3 (Cons 4 Nil)) `shouldBe` Nothing
        it "traverse 2" $
          traverse onlyThree (Cons 3 (Cons 3 Nil)) `shouldBe` Just (Cons 3 (Cons 3 Nil))
  describe "properties" $ do
    describe "Identity" $ checkLaws Identity Identity
    describe "List" $ checkLaws (flip Cons Nil) (flip Cons Nil)
    describe "Tree" $ checkLaws Leaf Leaf
    describe "S" $ do
      it "follows Functor laws" $
        quickBatch $ functor $ S [functorValue] functorValue
      it "follows Traversable laws" $
        quickBatch $ traversable $ S [traversableValue] traversableValue

functorValue :: (Int, String, Double)
functorValue = (123, "abc", 234.5)

traversableValue :: (Int, String, [Int])
traversableValue = (123, "abc", [123])

checkLaws f g = do
  it "follows Functor laws" $
    quickBatch $ functor $ f functorValue
  it "follows Traversable laws" $
    quickBatch $ traversable $ g traversableValue

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency
    [ (1, return Nil)
    , (1, Cons <$> arbitrary <*> arbitrary)
    ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency
    [ (1, return Empty)
    , (1, Leaf <$> arbitrary)
    , (1, Node <$> arbitrary <*> arbitrary <*> arbitrary)
    ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq
