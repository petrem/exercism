{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Spiral (spiral)

import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "spiral" $ do

  it "empty spiral" $
    spiral 0 `shouldBe` []

  it "trivial spiral" $
    spiral 1 `shouldBe` [ [1] ]

  it "spiral of side length 2" $
    spiral 2 `shouldBe` [ [1, 2]
                        , [4, 3] ]

  it "spiral of side length 3" $
    spiral 3 `shouldBe` [ [1, 2, 3]
                        , [8, 9, 4]
                        , [7, 6, 5] ]

  it "spiral of side length 4" $
    spiral 4 `shouldBe` [ [ 1,  2,  3, 4]
                        , [12, 13, 14, 5]
                        , [11, 16, 15, 6]
                        , [10,  9,  8, 7] ]

  it "spiral of side length 5" $
    spiral 5 `shouldBe` [ [ 1,  2,  3,  4, 5]
                        , [16, 17, 18, 19, 6]
                        , [15, 24, 25, 20, 7]
                        , [14, 23, 22, 21, 8]
                        , [13, 12, 11, 10, 9] ]

  it "spiral of side length 6" $
    spiral 6 `shouldBe` [ [ 1,   2,  3,  4, 5,   6]
                        , [ 20, 21, 22, 23, 24,  7]
                        , [ 19, 32, 33, 34, 25,  8]
                        , [ 18, 31, 36, 35, 26,  9]
                        , [ 17, 30, 29, 28, 27, 10]
                        , [ 16, 15, 14, 13, 12, 11] ]


  it "spiral of side length 7" $
    spiral 7 `shouldBe` [ [ 1,  2,  3,  4,  5,  6,  7]
                        , [24, 25, 26, 27, 28, 29,  8]
                        , [23, 40, 41, 42, 43, 30,  9]
                        , [22, 39, 48, 49, 44, 31, 10]
                        , [21, 38, 47, 46, 45, 32, 11]
                        , [20, 37, 36, 35, 34, 33, 12]
                        , [19, 18, 17, 16, 15, 14, 13] ]
