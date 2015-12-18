{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import Test.HUnit
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Web.Routes

newtype ArticleId = ArticleId Int deriving (Eq, Show, Num, PathInfo, Arbitrary)

data Sitemap
    = Home
    | Article ArticleId
    deriving (Eq, Show, Generic)

instance PathInfo Sitemap

instance Arbitrary Sitemap where
    arbitrary = oneof [return Home, fmap Article arbitrary]

prop_PathInfo_isomorphism :: Sitemap -> Bool
prop_PathInfo_isomorphism = pathInfoInverse_prop

case_toPathInfo :: Assertion
case_toPathInfo =
    do toPathInfo Home @?= "/home"
       toPathInfo (Article 0) @?= "/article/0"

case_fromPathInfo :: Assertion
case_fromPathInfo =
    do fromPathInfo "/home" @?= Right Home
       fromPathInfo "/article/0" @?= Right (Article 0)
       case fromPathInfo "/" :: Either String Sitemap of
         Left _ -> return ()
         url    -> assertFailure $ "expected a Left, but got: " ++ show url

main :: IO ()
main = hspec $ do
 prop "toPathInfo" case_toPathInfo
 prop "fromPathInfo" case_fromPathInfo
 prop "PathInfo_isomorphism"  prop_PathInfo_isomorphism
