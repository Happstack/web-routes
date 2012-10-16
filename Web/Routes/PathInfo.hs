{-# LANGUAGE CPP, FlexibleInstances, TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ > 702
{-# LANGUAGE DefaultSignatures, OverloadedStrings, TypeOperators #-}
#endif

module Web.Routes.PathInfo
    ( stripOverlap
    , stripOverlapBS
    , stripOverlapText
    , URLParser
    , pToken
    , segment
    , anySegment
    , patternParse
    , parseSegments
    , PathInfo(..)
    , toPathInfo
    , toPathInfoParams
    , fromPathInfo
    , mkSitePI
    , showParseError
#if __GLASGOW_HASKELL__ > 702
    , Generic
#endif
    ) where

import Blaze.ByteString.Builder (Builder, toByteString)
import Control.Applicative ((<$>), (<*))
import Control.Monad (msum)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List as List (stripPrefix, tails)
import Data.Text as Text (Text, pack, unpack, null, tails, stripPrefix)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal, signed)
import Data.Maybe (fromJust)
import Network.HTTP.Types
import Text.ParserCombinators.Parsec.Combinator (notFollowedBy)
import Text.ParserCombinators.Parsec.Error (ParseError, errorPos, errorMessages, showErrorMessages)
import Text.ParserCombinators.Parsec.Pos   (incSourceLine, sourceName, sourceLine, sourceColumn)
import Text.ParserCombinators.Parsec.Prim  ((<?>), GenParser, getInput, setInput, getPosition, token, parse, many)
import Web.Routes.Base (decodePathInfo, encodePathInfo)
import Web.Routes.Site (Site(..))

#if __GLASGOW_HASKELL__ > 702
import Control.Applicative ((<$), (<*>), (<|>), pure)
import GHC.Generics
#endif

-- this is not very efficient. Among other things, we need only consider the last 'n' characters of x where n == length y.
stripOverlap :: (Eq a) => [a] -> [a] -> [a]
stripOverlap x y = fromJust $ msum $ [ List.stripPrefix p y | p <- List.tails x]

stripOverlapText :: Text -> Text -> Text
stripOverlapText x y = fromJust $ msum $ [ Text.stripPrefix p y | p <- Text.tails x ]

stripOverlapBS :: B.ByteString -> B.ByteString -> B.ByteString
stripOverlapBS x y = fromJust $ msum $ [ stripPrefix p y | p <- B.tails x ] -- fromJust will never fail
    where
      stripPrefix :: B.ByteString -> B.ByteString -> Maybe B.ByteString
      stripPrefix x y
          | x `B.isPrefixOf` y = Just $ B.drop (B.length x) y
          | otherwise        = Nothing


type URLParser a = GenParser Text () a

pToken :: tok -> (Text -> Maybe a) -> URLParser a
pToken msg f = do pos <- getPosition
                  token unpack (const $ incSourceLine pos 1) f

-- | match on a specific string
segment :: Text -> URLParser Text
segment x = (pToken (const x) (\y -> if x == y then Just x else Nothing)) <?> unpack x

-- | match on any string
anySegment :: URLParser Text
anySegment = pToken (const "any string") Just

-- | Only matches if all segments have been consumed
eof :: URLParser ()
eof = notFollowedBy anySegment <?> "end of input"

-- | apply a function to the remainder of the segments
--
-- useful if you want to just do normal pattern matching:
-- >
-- > foo ["foo", "bar"] = Right (Foo Bar)
-- > foo ["baz"]        = Right Baz
-- > foo _              = Left "parse error"
--
-- > patternParse foo
patternParse :: ([Text] -> Either String a) -> URLParser a
patternParse p =
  do segs <- getInput
     case p segs of
       (Right r) ->
         do setInput []
            return r
       (Left err) -> fail err

-- | show Parsec 'ParseError' using terms that relevant to parsing a url
showParseError :: ParseError -> String
showParseError pErr =
  let pos    = errorPos pErr
      posMsg = sourceName pos ++ " (segment " ++ show (sourceLine pos) ++ " character " ++ show (sourceColumn pos) ++ "): "
      msgs   = errorMessages pErr
  in posMsg ++ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" msgs

-- | run a 'URLParser' on a list of path segments
--
-- returns @Left "parse error"@ on failure.
--
-- returns @Right a@ on success
parseSegments :: URLParser a -> [Text] -> Either String a
parseSegments p segments =
  case parse (p <* eof) (show segments) segments of
    (Left e)  -> Left (showParseError e)
    (Right r) -> Right r

{-

This requires parsec 3, can't figure out how to do it in parsec 2 yet.

p2u :: Parser a -> URLParser a
p2u p =
  mkPT $ \state@(State sInput sPos sUser) ->
  case sInput of
    (s:ss) ->
       do r <- runParsecT p (State s sPos sUser)
          return (fmap (fmap (fixReply ss)) r)

    where
      fixReply :: [String] -> (Reply String u a) -> (Reply [String] u a)
      fixReply _ (Error err) = (Error err)
      fixReply ss (Ok a (State "" sPos sUser) e) = (Ok a (State ss sPos sUser) e)
      fixReply ss (Ok a (State s sPos sUser) e) = (Ok a (State (s:ss) sPos sUser) e)
-}

{-
p2u :: Parser a -> URLParser a
p2u p =
  do (State sInput sPos sUser) <- getParserState
     case sInput of
       (s:ss) -> let r = runParser p () "" s
                 in case r of
                      (Left e) -> return e
-}

{-
  mkPT $ \state@(State sInput sPos sUser) ->
  case sInput of
    (s:ss) ->
       do r <- runParsecT p (State s sPos sUser)
          return (fmap (fmap (fixReply ss)) r)

    where
      fixReply :: [String] -> (Reply String u a) -> (Reply [String] u a)
      fixReply _ (Error err) = (Error err)
      fixReply ss (Ok a (State "" sPos sUser) e) = (Ok a (State ss sPos sUser) e)
      fixReply ss (Ok a (State s sPos sUser) e) = (Ok a (State (s:ss) sPos sUser) e)
-}

#if __GLASGOW_HASKELL__ > 702

class GPathInfo f where
  gtoPathSegments :: f url -> [Text]
  gfromPathSegments :: URLParser (f url)

instance GPathInfo U1 where
  gtoPathSegments U1 = []
  gfromPathSegments = pure U1

instance GPathInfo a => GPathInfo (M1 i c a) where
  gtoPathSegments = gtoPathSegments . unM1
  gfromPathSegments = M1 <$> gfromPathSegments

instance (GPathInfo a, GPathInfo b) => GPathInfo (a :*: b) where
  gtoPathSegments (a :*: b) = gtoPathSegments a ++ gtoPathSegments b
  gfromPathSegments = (:*:) <$> gfromPathSegments <*> gfromPathSegments

instance (GPathInfo a, GPathInfo b) => GPathInfo (a :+: b) where
  gtoPathSegments (L1 x) = "0" : gtoPathSegments x
  gtoPathSegments (R1 x) = "1" : gtoPathSegments x
  gfromPathSegments = L1 <$ segment "0" <*> gfromPathSegments
                  <|> R1 <$ segment "1" <*> gfromPathSegments

instance PathInfo a => GPathInfo (K1 i a) where
  gtoPathSegments = toPathSegments . unK1
  gfromPathSegments = K1 <$> fromPathSegments

#endif

class PathInfo url where
  toPathSegments :: url -> [Text]
  fromPathSegments :: URLParser url

#if __GLASGOW_HASKELL__ > 702
  default toPathSegments :: (Generic url, GPathInfo (Rep url)) => url -> [Text]
  toPathSegments = gtoPathSegments . from
  default fromPathSegments :: (Generic url, GPathInfo (Rep url)) => URLParser url
  fromPathSegments = to <$> gfromPathSegments
#endif

-- |convert url into the path info portion of a URL
toPathInfo :: (PathInfo url) => url -> Text
toPathInfo =  decodeUtf8 . toByteString . toPathInfoUtf8

-- |convert url into the path info portion of a URL
toPathInfoUtf8 :: (PathInfo url) => url -> Builder
toPathInfoUtf8 =  flip encodePath [] . toPathSegments

-- |convert url + params into the path info portion of a URL + a query string
toPathInfoParams :: (PathInfo url) =>
                    url -- ^ url
                 -> [(Text, Maybe Text)] -- ^ query string parameter
                 -> Text
toPathInfoParams url params = encodePathInfo (toPathSegments url) params

-- should this fail if not all the input was consumed?
--
-- in theory we
-- require the pathInfo to have the initial '/', but this code will
-- still work if it is missing.
--

-- If there are multiple //// at the beginning, we only drop the first
-- one, because we only added one in toPathInfo. Hence the others
-- should be significant.
--
-- However, if the pathInfo was prepend with http://example.org/ with
-- a trailing slash, then things might not line up.

-- | parse a 'String' into 'url' using 'PathInfo'.
--
-- returns @Left "parse error"@ on failure
--
-- returns @Right url@ on success

fromPathInfo :: (PathInfo url) => ByteString -> Either String url
fromPathInfo pi =
  parseSegments fromPathSegments (decodePathInfo $ dropSlash pi)
  where
    dropSlash s =
        if ((B.singleton '/') `B.isPrefixOf` s)
        then B.tail s
        else s

-- | turn a routing function into a 'Site' value using the 'PathInfo' class
mkSitePI :: (PathInfo url) =>
            ((url -> [(Text, Maybe Text)] -> Text) -> url -> a) -- ^ a routing function
         -> Site url a
mkSitePI handler =
  Site { handleSite         = handler
       , formatPathSegments = (\x -> (x, [])) . toPathSegments
       , parsePathSegments  = parseSegments fromPathSegments
       }

-- it's instances all the way down

instance PathInfo Text where
  toPathSegments = (:[])
  fromPathSegments = anySegment

instance PathInfo [Text] where
  toPathSegments = id
  fromPathSegments = many anySegment

instance PathInfo String where
  toPathSegments = (:[]) . pack
  fromPathSegments = unpack <$> anySegment

instance PathInfo [String] where
  toPathSegments = id . map pack
  fromPathSegments = many (unpack <$> anySegment)

instance PathInfo Int where
  toPathSegments i = [pack $ show i]
  fromPathSegments = pToken (const "Int") checkInt
   where checkInt txt =
           case signed decimal txt of
             (Left e) -> Nothing
             (Right (n, r))
                 | Text.null r -> Just n
                 | otherwise -> Nothing

instance PathInfo Integer where
  toPathSegments i = [pack $ show i]
  fromPathSegments = pToken (const "Integer") checkInt
   where checkInt txt =
           case signed decimal txt of
             (Left e) -> Nothing
             (Right (n, r))
                 | Text.null r -> Just n
                 | otherwise -> Nothing

