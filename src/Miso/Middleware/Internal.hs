module Miso.Middleware.Internal
  ( mapSubAction
  ) where

import Miso

mapSubAction :: (a -> b) -> Sub a m -> Sub b m
mapSubAction f sub = \readModel sink -> sub readModel (\a -> sink (f a))
