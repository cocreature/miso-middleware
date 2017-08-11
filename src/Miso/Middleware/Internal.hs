module Miso.Middleware.Internal
  ( fmapView
  , mapSubAction
  ) where

import Miso

fmapView :: (a -> b) -> View a -> View b
fmapView f (View v) = View (\sink -> v (\a -> sink (f a)))

mapSubAction :: (a -> b) -> Sub a m -> Sub b m
mapSubAction f sub = \readModel sink -> sub readModel (\a -> sink (f a))
