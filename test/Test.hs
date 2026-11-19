{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import Data.Text (Text)
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

case_toPathInfoParams :: Assertion
case_toPathInfoParams =
    do toPathInfoParams Home [("q",Just "1"),("r",Just "2")] @?= "/home?q=1&r=2"
       toPathInfoParams (Article 0) [("q",Just "1"),("r",Just "2")] @?= "/article/0?q=1&r=2"

case_fromPathInfo :: Assertion
case_fromPathInfo =
    do fromPathInfo "/home" @?= Right Home
       fromPathInfo "/article/0" @?= Right (Article 0)
       case fromPathInfo "/" :: Either String Sitemap of
         Left _ -> return ()
         url    -> assertFailure $ "expected a Left, but got: " ++ show url

case_fromPathInfoParams :: Assertion
case_fromPathInfoParams =
    do fromPathInfoParams "/home?q=1&r=2" @?= Right (Home, [("q",Just "1"),("r",Just "2")])
       fromPathInfoParams "/article/0?q=1&r=2" @?= Right (Article 0, [("q",Just "1"),("r",Just "2")])
       case fromPathInfoParams "/?q=1&r=2" :: Either String (Sitemap, [(Text, Maybe Text)]) of
         Left _ -> return ()
         url    -> assertFailure $ "expected a Left, but got: " ++ show url

-- the root: no path segments
root :: [Text]
root = []

case_toAbsolutePathInfo :: Assertion
case_toAbsolutePathInfo =
    do -- toPathInfo and toPathInfoParams are as they were: the root is ""
       toPathInfo root @?= ""
       toPathInfoParams root [("q",Just "1")] @?= "?q=1"
       -- the absolute versions give "/" for it, and the same for the rest
       toAbsolutePathInfo root @?= "/"
       toAbsolutePathInfoParams root [("q",Just "1")] @?= "/?q=1"
       toAbsolutePathInfo Home @?= "/home"
       toAbsolutePathInfoParams (Article 0) [("q",Just "1")] @?= "/article/0?q=1"

case_fromPathInfoText :: Assertion
case_fromPathInfoText =
    do fromPathInfoText "/home" @?= Right Home
       fromPathInfoText "/article/0" @?= Right (Article 0)
       fromPathInfoParamsText "/article/0?q=1&r=2" @?= Right (Article 0, [("q",Just "1"),("r",Just "2")])
       case fromPathInfoText "/" :: Either String Sitemap of
         Left _ -> return ()
         url    -> assertFailure $ "expected a Left, but got: " ++ show url

case_dropTrailingSlash :: Assertion
case_dropTrailingSlash =
    do -- fromPathInfo keeps a trailing slash, as a last, empty segment
       case fromPathInfo "/home/" :: Either String Sitemap of
         Left _ -> return ()
         url    -> assertFailure $ "expected a Left, but got: " ++ show url
       fromPathInfo (dropTrailingSlash "/home/") @?= Right Home
       fromPathInfoParams (dropTrailingSlash "/article/0/?q=1") @?= Right (Article 0, [("q",Just "1")])
       dropTrailingSlash "/home/" @?= "/home"
       dropTrailingSlash "/home/?q=1" @?= "/home?q=1"
       dropTrailingSlash "/home" @?= "/home"
       dropTrailingSlash "/" @?= "/"
       dropTrailingSlash "" @?= ""
       dropTrailingSlashText "/home/" @?= "/home"
       dropTrailingSlashText "/home/?q=1" @?= "/home?q=1"
       dropTrailingSlashText "/" @?= "/"
       fromPathInfoText (dropTrailingSlashText "/article/0/") @?= Right (Article 0)

main :: IO ()
main = hspec $ do
 prop "toPathInfo" case_toPathInfo
 prop "toPathInfoParams" case_toPathInfoParams
 prop "fromPathInfo" case_fromPathInfo
 prop "fromPathInfoParams" case_fromPathInfoParams
 prop "PathInfo_isomorphism" prop_PathInfo_isomorphism
 prop "toAbsolutePathInfo" case_toAbsolutePathInfo
 prop "fromPathInfoText" case_fromPathInfoText
 prop "dropTrailingSlash" case_dropTrailingSlash
