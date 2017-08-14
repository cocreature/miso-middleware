{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Miso.Middleware.Debugger
  ( withDebugger
  ) where

import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Miso
import           Miso.String (ms, MisoString)
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
renderDebugger m =
  div_
    [style_ (Map.fromList [("display", "flex")])]
    [ Svg.svg_
        [ width_ "800"
        , height_ "300"
        , style_ (Map.fromList [("border-style", "solid")])
        ]
        [ drawTree
            (RoseTree
               ()
               [ RoseTree () [RoseTree () [], RoseTree () [], RoseTree () []]
               , RoseTree () []
               , RoseTree () [RoseTree () [], RoseTree () [], RoseTree () []]
               ])
        ]
    , div_ [] [text (ms (show (extractModel m)))]
    ]

withPositions :: RoseTree (WithLocation a) -> RoseTree ((Double, Double), a)
withPositions tree = withPos 0 tree
  where
    h = height tree
    Bounds leftBound rightBound = treeBounds tree
    withPos !level (RoseTree (WithLocation v xPos) cs) =
      RoseTree ((x, y), v) (map (withPos (level + 1)) cs)
      where
        x = 70 * (xPos - leftBound) / (rightBound - leftBound) + 15
        y = 95 * (level + 1) / (fromIntegral h + 2) + 2.5

drawTree :: RoseTree model -> View (DebuggerAction action)
drawTree tree = Svg.g_ [] (drawTree' locatedTree)
  where
    locatedTree = withPositions (makeAbsolute (design tree))
    drawTree' (RoseTree ((x, y), _) cs) =
      lines ++
      Svg.circle_
        [Svg.r_ "4", Svg.cx_ (ms (show x) <> "%"), Svg.cy_ (ms (show y) <> "%")]
        [] :
      (drawTree' =<< cs)
      where
        lines =
          map
            (\(RoseTree ((x', y'), _) _) ->
               Svg.line_
                 [ Svg.x1_ (ms (show x) <> "%")
                 , Svg.y1_ (ms (show y) <> "%")
                 , Svg.x2_ (ms (show x') <> "%")
                 , Svg.y2_ (ms (show y') <> "%")
                 , Svg.stroke_ "black"
                 , Svg.strokeWidth_ "2"
                 ]
                 [])
            cs

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
