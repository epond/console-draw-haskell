module BucketFillSpec where

import Test.Hspec
import BucketFill
import Common
import Canvas
import qualified Data.Set as Set

nodeA = Coordinates 1 1
nodeB = Coordinates 1 2

spec :: Spec
spec = do
    describe "pushNode" $ do
        it "Given an empty canvas then always return the original node stack" $ do
            pushNode emptyCanvas Set.empty nodeA [] `shouldBe` []
            pushNode emptyCanvas Set.empty nodeA [nodeB] `shouldBe` [nodeB]
            pushNode emptyCanvas (Set.singleton nodeA) nodeA [] `shouldBe` []
        it "Given valid canvas and layerPoints containing node then return original node stack" $ do
            let blankCanvas = createNewCanvas 2 2
            pushNode blankCanvas (Set.singleton nodeA) nodeA [nodeB] `shouldBe` [nodeB]
        it "Given valid canvas and layerPoints without node then return stack with new node pushed" $ do
            let blankCanvas = createNewCanvas 2 2
            pushNode blankCanvas (Set.singleton nodeB) nodeA [nodeB] `shouldBe` [nodeA, nodeB]
        it "Works as expected when curried and composed" $ do
            let blankCanvas = createNewCanvas 2 2
            let push = pushNode blankCanvas (Set.singleton (Coordinates 2 2))
            let stack = push (Coordinates 2 1) . push (Coordinates 1 2) $ []
            stack `shouldBe` [(Coordinates 2 1), (Coordinates 1 2)]
    describe "bucketFill" $ do
        it "Given an empty canvas then always return empty ColourLayer" $ do
            bucketFill nodeA 'o' emptyCanvas `shouldBe` (ColourLayer [] 'o')
        it "Given a 1x1 blank canvas and empty node then return full ColourLayer" $ do
            let blankCanvas = createNewCanvas 1 1
            let fillLayer = bucketFill nodeA 'o' blankCanvas
            let layerPoints = Set.fromList $ lPoints fillLayer
            length (lPoints fillLayer) `shouldBe` 1
            Set.member (Coordinates 1 1) layerPoints `shouldBe` True
        it "Given a 2x2 blank canvas and empty node then return full ColourLayer" $ do
            let blankCanvas = createNewCanvas 2 2
            let fillLayer = bucketFill nodeA 'o' blankCanvas
            let layerPoints = Set.fromList $ lPoints fillLayer
            length (lPoints fillLayer) `shouldBe` 4
            Set.member (Coordinates 1 1) layerPoints `shouldBe` True
            Set.member (Coordinates 1 2) layerPoints `shouldBe` True
            Set.member (Coordinates 2 1) layerPoints `shouldBe` True
            Set.member (Coordinates 2 2) layerPoints `shouldBe` True