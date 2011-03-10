{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Web.Routes.PathInfo where

import Control.Applicative (pure, (*>),(<*>))
import Control.Monad (msum)
import Data.List (stripPrefix, tails)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec.Error (ParseError, errorPos, errorMessages, showErrorMessages)
import Text.ParserCombinators.Parsec.Pos   (incSourceLine, sourceName, sourceLine, sourceColumn)
import Text.ParserCombinators.Parsec.Prim  ((<?>), GenParser, getInput, setInput, pzero,getPosition, token, parse, many)
import Web.Routes.Base (decodePathInfo, encodePathInfo)
import Web.Routes.Site (Site(..))

-- this is not very efficient. Among other things, we need only consider the last 'n' characters of x where n == length y.
-- CB - added length based cutoff
stripOverlap :: (Eq a) => [a] -> [a] -> [a]
stripOverlap xs ys = fromJust $ msum $ [ stripPrefix p ys | p <- tails xs']
    where xs' = drop ((length xs - length ys `max` 0)) xs

stripOverlap' :: (Eq a) => [a] -> [a] -> [a]
stripOverlap' x y = fromJust $ msum $ [ stripPrefix p y | p <- tails x]

prop_stripOverlap :: [Int] -> [Int] -> Bool
prop_stripOverlap xs ys = stripOverlap xs ys == stripOverlap' xs ys

type URLParser a = GenParser String () a

pToken :: tok -> (String -> Maybe a) -> URLParser a
pToken msg f = do pos <- getPosition
                  token id (const $ incSourceLine pos 1) f

-- | match on a specific string
segment :: String -> URLParser String
segment x = (pToken (const x) (\y -> if x == y then Just x else Nothing)) <?> x

-- | match on any string
anySegment :: URLParser String
anySegment = pToken (const "any string") Just

-- | apply a function to the remainder of the segments
--
-- useful if you want to just do normal pattern matching:
-- >
-- > foo ["foo", "bar"] = Right (Foo Bar)
-- > foo ["baz"]        = Right Baz
-- > foo _              = Left "parse error"
-- 
-- > patternParse foo
patternParse :: ([String] -> Either String a) -> URLParser a
patternParse p =
  do segs <- getInput
     case p segs of
       (Right r) ->
         do setInput []
            return r
       (Left err) -> fail err
       
parseSegments :: URLParser a -> [String] -> Either String a
parseSegments p segments = 
  case parse p (show segments) segments of
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
class PathInfo a where
  toPathSegments :: a -> [String]
  fromPathSegments :: URLParser a

toPathInfo :: (PathInfo u) => u -> String
toPathInfo = ('/' :) . flip encodePathInfo [] . toPathSegments

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
fromPathInfo :: (PathInfo u) => String -> Either String u
fromPathInfo pi = 
  parseSegments fromPathSegments (decodePathInfo $ dropSlash pi) 
  where
    dropSlash ('/':rs) = rs
    dropSlash x        = x
    
mkSitePI :: (PathInfo url) => ((url -> [(String, String)] -> String) -> url -> a) -> Site url a
mkSitePI handler =
  Site { handleSite         = handler
       , formatPathSegments = (\x -> (x, [])) . toPathSegments
       , parsePathSegments  = parseSegments fromPathSegments
       }

showParseError :: ParseError -> String
showParseError pErr =
  let pos    = errorPos pErr
      posMsg = sourceName pos ++ " (segment " ++ show (sourceLine pos) ++ " character " ++ show (sourceColumn pos) ++ "): "
      msgs   = errorMessages pErr
  in posMsg ++ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" msgs

-- it's instances all the way down

instance PathInfo [String] where  
  toPathSegments = id
  fromPathSegments = many anySegment
  
instance PathInfo String where
  toPathSegments = (:[])
  fromPathSegments = anySegment
  
instance PathInfo Int where  
  toPathSegments i = [show i]
  fromPathSegments = pToken (const "int") checkInt
   where checkInt str = 
           case reads str of
             [(n,[])] -> Just n
             _ ->        Nothing
             
instance PathInfo Integer where  
  toPathSegments i = [show i]
  fromPathSegments = pToken (const "integer") checkInteger
   where checkInteger str = 
           case reads str of
             [(n,[])] -> Just n
             _ ->        Nothing             
