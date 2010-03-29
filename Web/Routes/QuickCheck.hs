module Web.Routes.QuickCheck where

import Web.Routes.PathInfo (PathInfo, toPathInfo, fromPathInfo)

pathInfoInverse_prop :: (Eq url, PathInfo url) => url -> Bool
pathInfoInverse_prop url =
    case (fromPathInfo $ toPathInfo url) of
      Right url' -> url == url'
      _ -> False
