module Task5Spec
  ( spec_incrementToString
  , spec_log2ToString
  , spec_gcdToString
  ) where

import Task4 (increment, log2, euclid)
import Task5 (toString)

import Test.Tasty.Hspec (specify, Spec, shouldBe)

import Data.List (intercalate)

tab :: String -> String
tab = ('\t' :)

funBody :: [String] -> String
funBody = intercalate "\n" . map tab

incrementString :: String
incrementString = intercalate "\n"
  [ "function (v0) {"
  , funBody 
    [ "var v1 = 0;"
    , "v1 = (v0) + (1);"
    , "return v1;"
    ]
  , "}"
  ]

log2String :: String
log2String = intercalate "\n"
  [ "function (v0) {"
  , funBody 
    [ "var v1 = 0;"
    , "var v2 = 0;"
    , "v2 = 1;"
    , "v1 = 0;"
    , "while ((v0) > (v2)) {"
    ,  tab "v2 = (v2) + (v2);"
    ,  tab "v1 = (v1) + (1);"
    , "}"
    , "return v1;"
    ]
  , "}"
  ]

euclidString :: String
euclidString = intercalate "\n"
  [ "function (v0, v1) {"
  , funBody
    [ "var v2 = 0;"
    , "var v3 = 0;"
    , "var v4 = 0;"
    , "while ((v1) > (0)) {"
    , tab "v3 = (v0) / (v1);"
    , tab "v4 = (v0) - ((v3) * (v1));"
    , tab "v0 = v1;"
    , tab "v1 = v4;"
    , "}"
    , "v2 = v0;"
    , "return v2;"
    ]
  , "}"
  ]
spec_incrementToString :: Spec
spec_incrementToString = 
  specify "Increment to string" $ do
    toString increment `shouldBe` incrementString
  
spec_log2ToString :: Spec
spec_log2ToString =
    specify "Log2 to string" $ do
      toString log2 `shouldBe` log2String

spec_gcdToString :: Spec
spec_gcdToString =
    specify "Gcd to string" $ do
      toString euclid `shouldBe` euclidString