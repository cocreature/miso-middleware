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

newtype DebuggerModel action model =
  DebuggerModel (RoseZipper (model, Maybe action))
  deriving (Show, Eq, Ord)

data Direction
  = Up
  | Down !Int
  deriving (Show, Eq, Ord)

data DebuggerAction action
  = Move [Direction]
  | Other !action
  deriving (Show, Eq, Ord)

unfoldZipper :: RoseZipper a -> RoseTree a
unfoldZipper (RoseZipper t []) = t
unfoldZipper (RoseZipper t ((ls, v, rs):ps)) =
  unfoldZipper (RoseZipper (RoseTree v (ls ++ [t] ++ rs)) ps)

withZipperMoves :: RoseZipper a -> RoseZipper ([Direction], a)
withZipperMoves (RoseZipper t ps) =
  RoseZipper
    (withTreeMoves t)
    (zipWith
       (\(ls, p, rs) ups ->
          let (ls', rs') = withParentMoves (ls, rs)
              addUps (ms, v) = (ups ++ ms, v)
          in (map (fmap addUps) ls', (ups, p), map (fmap addUps) rs'))
       ps
       (iterate (Up :) [Up]))
  where
    withParentMoves ::
         ([RoseTree a], [RoseTree a])
      -> ([RoseTree ([Direction], a)], [RoseTree ([Direction], a)])
    withParentMoves (ls, rs) = (ls', rs')
      where
        ls' =
          zipWith
            (\l i -> (\(ms, v) -> (Down i : ms, v)) <$> withTreeMoves l)
            ls
            [0 ..]
        rs' =
          zipWith
            (\r i -> (\(ms, v) -> (Down i : ms, v)) <$> withTreeMoves r)
            rs
            [length ls + 1 ..]
withTreeMoves :: RoseTree a -> RoseTree ([Direction], a)
withTreeMoves (RoseTree v cs) =
  RoseTree
    ([], v)
    ((zipWith (\c i -> fmap (\(ms, v') -> (Down i : ms, v')) (withTreeMoves c)))
       cs
       [0 ..])

renderDebugger :: (Show model, Show action) => DebuggerModel action model -> View (DebuggerAction action)
renderDebugger m@(DebuggerModel tree) =
  div_
    [style_ (Map.fromList [("display", "flex")])]
    [ Svg.svg_
        [ width_ "400"
        , height_ "300"
        , style_ (Map.fromList [("border-style", "solid")])
        ]
        [drawTree (unfoldZipper (withZipperMoves (fmap snd tree)))]
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
        x = 20 * xPos
        y = 20 * level

drawTree :: Show action => RoseTree ([Direction], Maybe action) -> View (DebuggerAction action)
drawTree tree = Svg.svg_ [] (drawTree' locatedTree ++ drawFocused locatedTree)
  where
    locatedTree =
      fmap (\((x', y'), v) -> ((50 + x' - x, 50 + y' - y), v)) locatedTree'
    locatedTree' = withPositions (makeAbsolute (design tree))
    (x, y) = head (getFocused locatedTree')
    drawTree' (RoseTree ((x, y), (moves, _)) cs) =
      lines ++
      Svg.circle_
        [ Svg.r_ "10"
        , Svg.cx_ (ms (show x) <> "%")
        , Svg.cy_ (ms (show y) <> "%")
        , Svg.onClick (Move moves)
        ]
        [] :
      (drawTree' =<< cs)
      where
        lines =
          map
            (\(RoseTree ((x', y'), (_, act)) _) ->
               Svg.line_
                 [ Svg.x1_ (ms (show x) <> "%")
                 , Svg.y1_ (ms (show y) <> "%")
                 , Svg.x2_ (ms (show x') <> "%")
                 , Svg.y2_ (ms (show y') <> "%")
                 , Svg.stroke_ "black"
                 , Svg.strokeWidth_ "4"
                 ]
                 (case act of
                    Nothing -> []
                    Just act' -> [Svg.title_ [] [text (ms (show act'))]]))
            cs

getFocused :: RoseTree (a, ([Direction], model)) -> [a]
getFocused (RoseTree (a, ([], _)) _) =
  [a]
getFocused (RoseTree _ cs) =
  getFocused =<< cs

drawFocused :: RoseTree ((Double,Double), ([Direction], model)) -> [View (DebuggerAction action)]
drawFocused (RoseTree ((x, y), ([], _)) _) =
  [ Svg.circle_
      [ Svg.r_ "12"
      , Svg.cx_ (ms (show x) <> "%")
      , Svg.cy_ (ms (show y) <> "%")
      , Svg.strokeWidth_ "4"
      , Svg.stroke_ "orange"
      ]
      []
  ]
drawFocused (RoseTree _ cs) = drawFocused =<< cs

extractModel :: DebuggerModel action model -> model
extractModel (DebuggerModel (RoseZipper (RoseTree (m, _) _) _)) = m

applyMove :: [Direction] -> RoseZipper a -> RoseZipper a
applyMove [] t = t
applyMove (Up:ms) t =
  case moveUp t of
    Just t' -> applyMove ms t'
    Nothing -> t
applyMove (Down i:ms) t =
  case moveDown i t of
    Just t' -> applyMove ms t'
    Nothing -> t

withDebugger :: (Show model, Show action) => App model action -> App (DebuggerModel action model) (DebuggerAction action)
withDebugger (App model update view subs events initialAction) =
  App model' update' view' (map mapSub subs) events (Other initialAction)
  where
    model' = DebuggerModel (RoseZipper (RoseTree (model, Nothing) []) [])
    update' (Move ms) (DebuggerModel tree) =
      noEff (DebuggerModel (applyMove ms tree))
    update' (Other act) model@(DebuggerModel tree) =
      case update act (extractModel model) of
        Effect m' acts ->
          let tree' = insertAndMoveTo (RoseTree (m', Just act) []) tree
          in Effect (DebuggerModel tree') (map (fmap Other) acts)
    view' model =
      div_ [] [renderDebugger model, fmapView Other (view (extractModel model))]
    mapSub ::
         Sub action model
      -> Sub (DebuggerAction action) (DebuggerModel action model)
    mapSub sub =
      mapSubAction
        Other
        (\readModel sink -> sub (fmap extractModel readModel) sink)
