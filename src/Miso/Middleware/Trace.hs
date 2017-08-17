{-# LANGUAGE OverloadedStrings #-}
module Miso.Middleware.Trace
  ( TraceAction(..)
  , withTrace
  ) where

import Data.Monoid
import Miso

import Miso.Middleware.Internal

data TraceAction action
  = Traced
  | OtherTraceAction !action
  deriving (Show, Eq, Ord)

-- | Creates an 'App' that dumps all state transitions to the console
-- for debugging purposes.
withTrace :: (Show action, Show model) => App model action -> App model (TraceAction action)
withTrace (App model update view subs events initialAction) =
  App
    model
    update'
    (fmap OtherTraceAction . view)
    (map (mapSubAction OtherTraceAction) subs)
    events
    (OtherTraceAction initialAction)
  where
    update' Traced model = noEff model
    update' (OtherTraceAction action) model =
      case update action model of
        Effect model' acts ->
          Effect
            model'
            ((Traced <$ putStrLn (formatTrace action model model')) :
             map (fmap OtherTraceAction) acts)
    formatTrace ::
         (Show action, Show model) => action -> model -> model -> String
    formatTrace action oldModel newModel =
      "(" <> show action <> " ," <> show oldModel <> ") → " <> show newModel
