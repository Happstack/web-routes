{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
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
    do toPathInfo Home @?= "/0"
       toPathInfo (Article 0) @?= "/1/0"

case_fromPathInfo :: Assertion
case_fromPathInfo =
    do fromPathInfo "/0" @?= Right Home
       fromPathInfo "/1/0" @?= Right (Article 0)
       case fromPathInfo "/" :: Either String Sitemap of
         Left _ -> return ()
         url    -> assertFailure $ "expected a Left, but got: " ++ show url

main :: IO ()
main = $defaultMainGenerator
