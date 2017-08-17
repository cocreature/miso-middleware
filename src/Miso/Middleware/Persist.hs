{-# LANGUAGE OverloadedStrings #-}
module Miso.Middleware.Persist
  ( PersistAction(..)
  , withPersistentModel
  ) where

import Data.Monoid
import Data.Aeson
import Miso

import Miso.Middleware.Internal

data PersistAction model action
 = InitAction
 | LoadedModel !model
 | PersistedNoop
 | OtherAction !action
 deriving (Show, Eq, Ord)

loadModelWithDefault :: FromJSON model => model -> IO model
loadModelWithDefault defaultModel = do
  modelOrErr <- getLocalStorage "miso.persist.model"
  case modelOrErr of
    Left err -> putStrLn ("Couldn’t load model from local storage: " <> err) >> pure defaultModel
    Right model -> pure model

withPersistentModel :: (Eq model, ToJSON model, FromJSON model) => App model action -> App model (PersistAction model action)
withPersistentModel (App model update view subs events initialAction) =
  App model update' view' subs' events InitAction
  where
    update' InitAction model =
      model <# (LoadedModel <$> loadModelWithDefault model)
    update' PersistedNoop model = noEff model
    update' (LoadedModel model) _ = model <# pure (OtherAction initialAction)
    update' (OtherAction action) model =
      case update action model of
        Effect model' acts ->
          Effect
            model'
            ((PersistedNoop <$ setLocalStorage "miso.persist.model" model') :
             map (fmap OtherAction) acts)
    view' = \model -> fmap OtherAction (view model)
    subs' = map (mapSubAction OtherAction) subs
