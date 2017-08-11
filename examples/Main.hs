{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso
import Miso.Middleware.Persist
import Miso.Middleware.Trace
import Miso.String

type Model = Int

main :: IO ()
main = startApp (withTrace (withPersistentModel NoOp originalApp))

originalApp :: App Model Action
originalApp =
  App
  { model = 0
  , update = updateModel
  , view = viewModel
  , events = defaultEvents
  , subs = []
  , initialAction = SayHelloWorld
  }

updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do
  putStrLn "Hello World!" >> pure NoOp

data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

viewModel :: Int -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text $ ms (show x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]

