{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Miso.Middleware.Debugger
  ( withDebugger
  ) where

import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Miso
import           Miso.String (ms)
import qualified Miso.Svg as Svg

import           Miso.Middleware.Internal
import           Miso.Middleware.Internal.Tree

newtype DebuggerModel model =
  DebuggerModel (RoseZipper model)
  deriving (Show, Eq, Ord)

data DebuggerAction action
  = MoveDown !Int
  | MoveUp
  | Other !action
  deriving (Show, Eq, Ord)

renderDebugger :: Show model => DebuggerModel model -> View (DebuggerAction action)
renderDebugger m@(DebuggerModel (RoseZipper (RoseTree _ cs) ps)) =
  div_
    [style_ (Map.fromList [("display", "flex")])]
    [ Svg.svg_
        [ width_ "800"
        , height_ "300"
        , style_ (Map.fromList [("border-style", "solid")])
        ]
        (parents ++
         (Svg.circle_
            [ Svg.cx_ "50%"
            , Svg.cy_ "50%"
            , Svg.r_ "30"
            , Svg.fill_ "grey"
            , Svg.onClick MoveUp
            ]
            [] :
          zipWith drawChild cs [0 ..]))
    , div_ [] [text (ms (show (extractModel m)))]
    ]
  where
    drawChild _ i =
      Svg.circle_
        [ Svg.cy_ "75%"
        , Svg.cx_ (ms (show xPos) <> "%")
        , Svg.r_ "30"
        , Svg.fill_ "grey"
        , Svg.onClick (MoveDown i)
        ]
        []
      where
        delta = 1 / (fromIntegral (length cs) + 1)
        xPos = (fromIntegral i + 1) * delta * 100
    parents :: [View (DebuggerAction action)]
    parents =
      case ps of
        [] -> []
        (_:_) -> [drawParent]
    drawParent :: View (DebuggerAction action)
    drawParent =
      Svg.circle_
        [ Svg.cy_ "25%"
        , Svg.cx_ "50%"
        , Svg.r_ "30"
        , Svg.fill_ "grey"
        , Svg.onClick MoveUp
        ]
        []

extractModel :: DebuggerModel model -> model
extractModel (DebuggerModel (RoseZipper (RoseTree m _) _)) = m

withDebugger :: Show model => App model action -> App (DebuggerModel model) (DebuggerAction action)
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
