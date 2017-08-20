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

data DebuggerModel action model = DebuggerModel
  { debuggerHistory :: !(RoseZipper (model, Maybe action))
  , actionsSuspended :: !Bool
  } deriving (Show, Eq, Ord)

data Direction
  = Up
  | Down !Int
  deriving (Show, Eq, Ord)

data DebuggerAction action
  = Move [Direction]
  | ToggleSuspend
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

css :: MisoString
css =
  ".debugger {\
  \  position: fixed;\
  \  right: 0;\
  \  top: 0;\
  \  display: flex;\
  \  flex-direction: column;\
  \  align-items: stretch;\
  \}\
  \.debugger svg {\
  \  align-self: center;\
  \}\
  \.debugger-controls {\
  \  display: flex;\
  \  flex-direction: column;\
  \}\
  \.debugger-navigation {\
  \  display: flex;\
  \  justify-content: space-around;\
  \}\
  \.debugger-suspend {\
  \  display: flex;\
  \  justify-content: center;\
  \}\
  \.debugger-controls button {\
  \  margin: 5px;\
  \}\
  \.model-state {\
  \  display: flex;\
  \  flex-direction: column;\
  \}\
  \.outer-debugger {\
  \  display: flex;\
  \}\
  \.debugger hr {\
  \  margin: 10px 0;\
  \}\
  \g {\
  \  transition: transform .3s;\
  \}"

renderDebugger :: (Show model, Show action) => DebuggerModel action model -> View (DebuggerAction action)
renderDebugger m@(DebuggerModel tree suspended) =
  div_
    []
    [ nodeHtml "style" [] [text css]
    , div_
        [class_ "debugger"]
        [ div_
            [class_ "model"]
            [ h3_ [] ["Model"]
            , div_ [class_ "model-value"] [text (ms (show (extractModel m)))]
            ]
        , hr_ [] []
        , renderStateTree tree
        , hr_ [] []
        , div_
            [class_ "debugger-controls"]
            [ div_ [class_ "debugger-navigation"] (map goBackButton [5, 10, 50])
            , div_
                [class_ "debugger-suspend"]
                [ button_
                    [onClick ToggleSuspend]
                    [ text
                        (if suspended
                           then "Resume"
                           else "Suppress actions")
                    ]
                ]
            ]
        ]
    ]
  where
    goBackButton n =
      button_
        [onClick (Move (replicate n Up))]
        [text ("Go back " <> show' n <> " states")]

renderStateTree :: (Show model, Show action) => RoseZipper (model, Maybe action) -> View (DebuggerAction action)
renderStateTree tree =
  Svg.svg_
    [width_ (show' width <> "px"), height_ (show' height <> "px")]
    [drawTree (width, height) (unfoldZipper (withZipperMoves (fmap snd tree)))]
  where
    width = 400
    height = 400

withPositions :: RoseTree (WithLocation a) -> RoseTree ((Double, Double), a)
withPositions tree = withPos 0 tree
  where
    h = height tree
    Bounds leftBound rightBound = treeBounds tree
    withPos !level (RoseTree (WithLocation v xPos) cs) =
      RoseTree ((x, y), v) (map (withPos (level + 1)) cs)
      where
        x = 50 * xPos
        y = 50 * level

show' :: Show a => a -> MisoString
show' = ms . show

getFocusedPos :: RoseZipper a -> (Double, Double)
getFocusedPos =
  head .
  getFocused .
  withPositions . makeAbsolute . design . unfoldZipper . withZipperMoves

drawTree :: Show action => (Int, Int) -> RoseTree ([Direction], Maybe action) -> View (DebuggerAction action)
drawTree (width, height) tree =
  Svg.g_
    [ style_
        (Map.singleton
           "transform"
           ("translate(" <> show' (fromIntegral width / 2 - x) <> "px, " <>
            show' (fromIntegral height / 2 - y) <>
            "px)"))
    ]
    (drawTree' locatedTree ++ drawFocused locatedTree)
  where
    locatedTree = withPositions (makeAbsolute (design tree))
    (x, y) = head (getFocused locatedTree)
    drawTree' (RoseTree ((x, y), (moves, _)) cs) =
      lines ++
      Svg.circle_
        [ Svg.r_ "10"
        , Svg.cx_ (show' x)
        , Svg.cy_ (show' y)
        , Svg.onClick (Move moves)
        ]
        [] :
      (drawTree' =<< cs)
      where
        lines =
          map
            (\(RoseTree ((x', y'), (_, act)) _) ->
               Svg.line_
                 [ Svg.x1_ (show' x)
                 , Svg.y1_ (show' y)
                 , Svg.x2_ (show' x')
                 , Svg.y2_ (show' y')
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
      , Svg.cx_ (show' x)
      , Svg.cy_ (show' y)
      , Svg.strokeWidth_ "4"
      , Svg.stroke_ "orange"
      ]
      []
  ]
drawFocused (RoseTree _ cs) = drawFocused =<< cs

extractModel :: DebuggerModel action model -> model
extractModel (DebuggerModel (RoseZipper (RoseTree (m, _) _) _) _) = m

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

-- | Adds a debugger to 'App' that records all state transitions and
-- provides an interface for viewing these transitions.
withDebugger :: (Show model, Show action) => App model action -> App (DebuggerModel action model) (DebuggerAction action)
withDebugger (App model update view subs events initialAction) =
  App model' update' view' (map mapSub subs) events (Other initialAction)
  where
    model' = DebuggerModel (RoseZipper (RoseTree (model, Nothing) []) []) False
    update' (Move ms) (DebuggerModel tree suspended) =
      noEff (DebuggerModel (applyMove ms tree) suspended)
    update' ToggleSuspend (DebuggerModel tree suspended) =
      noEff (DebuggerModel tree (not suspended))
    update' (Other _) model@(DebuggerModel _ True) = noEff model
    update' (Other act) model@(DebuggerModel tree False) =
      case update act (extractModel model) of
        Effect m' acts ->
          let tree' = addChild (RoseTree (m', Just act) []) tree
          in Effect
               (DebuggerModel tree' False)
               (pure (Move [Down 0]) : map (fmap Other) (acts))
    view' model =
      div_ [] [fmap Other (view (extractModel model)), renderDebugger model]
    mapSub ::
         Sub action model
      -> Sub (DebuggerAction action) (DebuggerModel action model)
    mapSub sub =
      mapSubAction
        Other
        (\readModel sink -> sub (fmap extractModel readModel) sink)
