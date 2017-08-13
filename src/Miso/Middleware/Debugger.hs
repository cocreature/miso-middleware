{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Miso.Middleware.Debugger
  ( withDebugger
  ) where

import Data.Maybe
import Miso

import Miso.Middleware.Internal
import Miso.Middleware.Internal.Tree

newtype DebuggerModel model =
  DebuggerModel (RoseZipper model)
  deriving (Show, Eq, Ord)

data DebuggerAction action
  = MoveDown !Int
  | MoveUp
  | Other !action
  deriving (Show, Eq, Ord)

renderDebugger :: DebuggerModel model -> View (DebuggerAction action)
renderDebugger _ =
  div_
    []
    [ button_ [onClick MoveUp] ["move up"]
    , button_ [onClick (MoveDown 0)] ["move down"]
    ]

extractModel :: DebuggerModel model -> model
extractModel (DebuggerModel (RoseZipper (RoseTree m _) _)) = m

withDebugger :: App model action -> App (DebuggerModel model) (DebuggerAction action)
withDebugger (App model update view subs events initialAction) =
  App model' update' view' (map mapSub subs) events (Other initialAction)
  where
    model' = DebuggerModel (RoseZipper (RoseTree model []) [])
    update' MoveUp (DebuggerModel tree) =
      noEff (DebuggerModel (fromMaybe tree (moveUp tree)))
    update' (MoveDown i) (DebuggerModel tree) =
      noEff (DebuggerModel (fromMaybe tree (moveDown i tree)))
    update' (Other act) model@(DebuggerModel tree) =
      case update act (extractModel model) of
        Effect m' acts ->
          let tree' = insertAndMoveTo (RoseTree m' []) tree
          in Effect (DebuggerModel tree') (map (fmap Other) acts)
    view' model =
      div_ [] [renderDebugger model, fmapView Other (view (extractModel model))]
    mapSub :: Sub action model -> Sub (DebuggerAction action) (DebuggerModel model)
    mapSub sub =
      mapSubAction
        Other
        (\readModel sink -> sub (fmap extractModel readModel) sink)
