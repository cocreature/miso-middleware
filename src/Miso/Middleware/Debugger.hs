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
renderDebugger m@(DebuggerModel (RoseZipper (RoseTree _ cs) ps)) =
  div_
    [style_ (Map.fromList [("display", "flex")])]
    [ Svg.svg_
        [ width_ "800"
        , height_ "300"
        , style_ (Map.fromList [("border-style", "solid")])
        ]
        (renderParents ps ++
         renderMainNode ++ concat (zipWith (drawChild childDelta) cs [0 ..]))
    , div_ [] [text (ms (show (extractModel m)))]
    ]
  where
    childDelta = 1 / (fromIntegral (length cs) + 1)

drawChild :: Double -> RoseTree model -> Int -> [View (DebuggerAction action)]
drawChild dist _ i =
  [ Svg.line_
      [ Svg.y1_ "75%"
      , Svg.x1_ (ms (show xPos) <> "%")
      , Svg.y2_ "50%"
      , Svg.x2_ "50%"
      , strokeWidth "2"
      , Svg.stroke_ "grey"
      ]
      []
  , Svg.circle_
      [ Svg.cy_ "75%"
      , Svg.cx_ (ms (show xPos) <> "%")
      , Svg.r_ parentRadius
      , Svg.fill_ "grey"
      , Svg.onClick (MoveDown i)
      ]
      []
  ]
  where
    xPos = (fromIntegral i + 1) * dist * 100

renderMainNode :: [View (DebuggerAction action)]
renderMainNode =
  [ Svg.circle_ [Svg.cx_ "50%", Svg.cy_ "50%", Svg.r_ "15", Svg.fill_ "grey"] []
  , Svg.circle_
      [ Svg.cx_ "50%"
      , Svg.cy_ "50%"
      , Svg.r_ "15"
      , Svg.fill_ "none"
      , Svg.stroke_ "orange"
      , Svg.strokeWidth_ "10"
      ]
      []
  ]

parentRadius :: MisoString
parentRadius = "10"

renderParents :: [([RoseTree model], model, [RoseTree model])] -> [View (DebuggerAction action)]
renderParents ps =
      Svg.line_
        [ Svg.x1_ "50%"
        , Svg.x2_ "50%"
        , Svg.y1_ "50%"
        , Svg.y2_ "35%"
        , strokeWidth "2"
        , Svg.stroke_ "grey"
        ]
        [] :
      concat (zipWith renderParent (take 3 ps) [35,25 ..]) ++
      if length ps <= 3
        then let y = ms (show (35 - (10 * length ps))) <> "%"
             in [ Svg.line_
                    [ Svg.x1_ "48%"
                    , Svg.x2_ "52%"
                    , Svg.y1_ y
                    , Svg.y2_ y
                    , strokeWidth "2"
                    , Svg.stroke_ "grey"
                    ]
                    []
                ]
        else []

renderParent :: ([RoseTree model], model, [RoseTree model]) -> Int -> [View (DebuggerAction action)]
renderParent (ls, _, rs) height =
  Svg.line_
    [ Svg.x1_ "50%"
    , Svg.x2_ "50%"
    , Svg.y1_ height'
    , Svg.y2_ (ms (show (height - 10)) <> "%")
    , strokeWidth "2"
    , Svg.stroke_ "grey"
    ]
    [] :
  Svg.circle_
    [ Svg.cy_ height'
    , Svg.cx_ "50%"
    , Svg.r_ parentRadius
    , Svg.fill_ "grey"
    , Svg.onClick MoveUp
    ]
    [] :
  leftArrow ++ rightArrow
  where
    height' = ms (show height) <> "%"
    heightArrowEnd = ms (show (height + 5)) <> "%"
    leftArrow
      | null ls = []
      | otherwise =
        [ Svg.line_
            [ Svg.x1_ "50%"
            , Svg.x2_ "35%"
            , Svg.y1_ height'
            , Svg.y2_ heightArrowEnd
            , strokeWidth "2"
            , Svg.stroke_ "grey"
            ]
            []
        ]
    rightArrow
      | null rs = []
      | otherwise =
        [ Svg.line_
            [ Svg.x1_ "50%"
            , Svg.x2_ "65%"
            , Svg.y1_ height'
            , Svg.y2_ heightArrowEnd
            , strokeWidth "2"
            , Svg.stroke_ "grey"
            ]
            []
        ]

strokeWidth :: MisoString -> Attribute action
strokeWidth = textProp "stroke-width"

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
