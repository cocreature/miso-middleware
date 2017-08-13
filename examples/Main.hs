{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso
import Miso.Middleware.Debugger
import Miso.Middleware.Persist
import Miso.Middleware.Trace
import Miso.String

type Model = Int

main :: IO ()
main = startApp (withTrace (withDebugger originalApp))

originalApp :: App Model Action
originalApp =
  App
  { model = 0
  , update = updateModel
  , view = viewModel
  , events = defaultEvents
  , subs = []
  , initialAction = NoOp
  }

updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m

data Action
  = AddOne
  | SubtractOne
  | NoOp
  deriving (Show, Eq)

viewModel :: Int -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text $ ms (show x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]

