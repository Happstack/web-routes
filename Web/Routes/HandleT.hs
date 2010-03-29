module Web.Routes.HandleT where

import Data.Monoid (Monoid(mempty, mappend))
import Web.Routes.Monad (RouteT, runRouteT)
import Text.Parsec.Error (ParseError)

data Site url a
    = Site { handleLink  :: (url -> String) -> url -> a
           , defaultPage :: url
           , formatLink  :: url -> String
           , parseLink   :: String -> Either String url
           }

withDefault :: Site url a -> String -> Either String url
withDefault site pathInfo
  | null pathInfo = Right (defaultPage site)
  | otherwise     = (parseLink site) pathInfo
                         
runSite :: String -> Site url a -> String -> (Either String a)
runSite approot site pathInfo =
  case (withDefault site) pathInfo of
    (Left errs) -> (Left errs)
    (Right url)  -> Right $ (handleLink site) (\url -> approot `mappend` (formatLink site url)) url
