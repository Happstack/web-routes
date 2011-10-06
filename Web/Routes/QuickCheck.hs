module Web.Routes.QuickCheck where

import qualified Data.Text.Encoding as Text
import Web.Routes.PathInfo (PathInfo, toPathInfo, fromPathInfo)

pathInfoInverse_prop :: (Eq url, PathInfo url) => url -> Bool
pathInfoInverse_prop url =
    case (fromPathInfo $ Text.encodeUtf8 $ toPathInfo url) of
      Right url' -> url == url'
      _ -> False
