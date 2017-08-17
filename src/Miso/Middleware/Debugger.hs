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

foreign import javascript unsafe "$r = performance.now();" now ::
               IO Double

data AnimationState = AnimationState
  { startPos :: !(Double, Double)
  , startTime :: !Double
  , animationPos :: !Double -- ^ Double between 0 and 1 indicating the
                            -- position in the animation
  } deriving (Show, Eq, Ord)

data DebuggerModel action model = DebuggerModel
  { debuggerHistory :: !(RoseZipper (model, Maybe action))
  , animationState :: !(Maybe AnimationState)
  } deriving (Show, Eq, Ord)

data Direction
  = Up
  | Down !Int
  deriving (Show, Eq, Ord)

data DebuggerAction action
  = Move [Direction]
  | StartAnimation [Direction] !Double
  | AnimationFrame !Double
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
  \.debugger-controls {\
  \  display: flex;\
  \  flex-direction: row;\
  \  justify-content: space-around;\
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
  \}"

renderDebugger :: (Show model, Show action) => DebuggerModel action model -> View (DebuggerAction action)
renderDebugger m@(DebuggerModel tree animationState) =
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
        , renderStateTree tree animationState
        , hr_ [] []
        , div_ [class_ "debugger-controls"] (map goBackButton [5, 10, 50])
        ]
    ]
  where
    goBackButton n =
      button_
        [onClick (Move (replicate n Up))]
        [text ("Go back " <> show' n <> " states")]

renderStateTree ::
  (Show model, Show action) =>
  RoseZipper (model, Maybe action) ->
  Maybe AnimationState ->
  View (DebuggerAction action)
renderStateTree tree animationState =
  Svg.svg_
    [width_ (show' width), height_ (show' height)]
    [ drawTree
        (width, height)
        animationState
        (unfoldZipper (withZipperMoves (fmap snd tree)))
    ]
  where
    width = 400
    height = 300

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

(*^) :: Num a => a -> (a, a) -> (a, a)
(*^) s (!x,!y) = (s * x, s * y)

(^+^) :: Num a => (a, a) -> (a, a) -> (a, a)
(!x,!y) ^+^ (!x',!y') = (x+x', y+y')

(^-^) :: Num a => (a,a) -> (a,a) -> (a,a)
(!x,!y) ^-^ (!x',!y') = (x-x',y-y')

infixl 7 *^
infixl 6 ^+^
infixl 6 ^-^

interpolate :: Maybe AnimationState -> (Double, Double) -> (Double, Double)
interpolate Nothing pos = pos
interpolate (Just (AnimationState start _ t)) pos =
  easeOut t start (pos^-^start)

easeIn :: Double -> (Double,Double) -> (Double,Double) -> (Double,Double)
easeIn t b c = (t * t) *^ c ^+^ b

easeOut :: Double -> (Double,Double) -> (Double,Double) -> (Double,Double)
easeOut t b c = (t * (2-t)) *^ c ^+^ b

drawTree :: Show action => (Int, Int) -> Maybe AnimationState -> RoseTree ([Direction], Maybe action) -> View (DebuggerAction action)
drawTree (width, height) animationState tree =
  Svg.g_
    [ Svg.transform_
        ("translate" <>
         show' (fromIntegral width / 2 - x, fromIntegral height / 2 - y))
    ]
    (drawTree' locatedTree ++ drawFocused locatedTree)
  where
    locatedTree = withPositions (makeAbsolute (design tree))
    (x, y) = interpolate animationState (head (getFocused locatedTree))
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

withDebugger :: (Show model, Show action) => App model action -> App (DebuggerModel action model) (DebuggerAction action)
withDebugger (App model update view subs events initialAction) =
  App model' update' view' (map mapSub subs) events (Other initialAction)
  where
    model' =
      DebuggerModel (RoseZipper (RoseTree (model, Nothing) []) []) Nothing
    update' (AnimationFrame now) (DebuggerModel tree Nothing) =
      noEff (DebuggerModel tree Nothing)
    update' (AnimationFrame now') (DebuggerModel tree (Just (AnimationState startPos startTime t)))
      | now' - startTime > 1000 * animationDuration =
        noEff (DebuggerModel tree Nothing)
      | otherwise =
        DebuggerModel
          tree
          (Just
             (AnimationState
                startPos
                startTime
                ((now' - startTime) / (1000 * animationDuration)))) <#
        (AnimationFrame <$> now)
    update' (Move ms) model = model <# (StartAnimation ms <$> now)
    update' (StartAnimation ms time) (DebuggerModel tree _) =
      let lastFocusedPos = getFocusedPos tree
      in DebuggerModel
           (applyMove ms tree)
           (Just (AnimationState lastFocusedPos time 0)) <#
         (AnimationFrame <$> now)
    update' (Other act) model@(DebuggerModel tree _) =
      case update act (extractModel model) of
        Effect m' acts ->
          let tree' = addChild (RoseTree (m', Just act) []) tree
          in Effect
               (DebuggerModel tree' Nothing)
               (pure (Move [Down 0]) : map (fmap Other) (acts))
    view' model =
      div_ [] [fmapView Other (view (extractModel model)), renderDebugger model]
    mapSub ::
         Sub action model
      -> Sub (DebuggerAction action) (DebuggerModel action model)
    mapSub sub =
      mapSubAction
        Other
        (\readModel sink -> sub (fmap extractModel readModel) sink)
    animationDuration = 0.3
