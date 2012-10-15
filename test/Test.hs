{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import Control.Applicative
import Test.Framework
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
    deriving (Eq, Show)

instance PathInfo Sitemap where
    toPathSegments Home = ["home"]
    toPathSegments (Article x) = "article" : toPathSegments x
    fromPathSegments = Home <$ segment "home"
                   <|> Article <$ segment "article" <*> fromPathSegments

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
main = $defaultMainGenerator
