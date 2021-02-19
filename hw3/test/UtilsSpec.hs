module UtilsSpec where

import Utils

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Path resolving" $ do
    it "resolving relative paths" $ do
      resolvePath "/" "a/b.exe" `shouldBe` Just "/a/b.exe"
      resolvePath "/t/e/" "a/k.exe" `shouldBe` Just "/t/e/a/k.exe"
    it "resolving absolute paths" $ do
      resolvePath "kfd" "/a/b" `shouldBe` Just "/a/b"
      resolvePath "/a/b" "/c" `shouldBe` Just "/c"
    it "resolving '.'" $ do
      resolvePath "/a" "." `shouldBe` Just "/a"
      resolvePath "/b/c" "././." `shouldBe` Just "/b/c"
    it "resolving '..'" $ do
      resolvePath "/a/" "./.." `shouldBe` Just "/"
      resolvePath "/a/../b" "c/.." `shouldBe` Just "/b"
      resolvePath "/a/b/../.." ".." `shouldBe` Nothing