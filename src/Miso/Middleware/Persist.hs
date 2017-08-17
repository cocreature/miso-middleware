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
 | OtherAction !action
 deriving (Show, Eq, Ord)

loadModelWithDefault :: FromJSON model => model -> IO model
loadModelWithDefault defaultModel = do
  modelOrErr <- getLocalStorage "miso.persist.model"
  case modelOrErr of
    Left err -> putStrLn ("Couldn’t load model from local storage: " <> err) >> pure defaultModel
    Right model -> pure model

withPersistentModel :: (Eq model, ToJSON model, FromJSON model) => action -> App model action -> App model (PersistAction model action)
withPersistentModel persistAction (App model update view subs events initialAction) =
  App model update' view' subs' events InitAction
  where
    update' InitAction model =
      model <# (LoadedModel <$> loadModelWithDefault model)
    update' (LoadedModel model) _ = model <# pure (OtherAction initialAction)
    update' (OtherAction action) model =
      case update action model of
        Effect model' acts
-- We need to check that the model has actually changed, otherwise we
-- get stuck in an infinite loop since after updating local storage,
-- 'update' will be called again with 'persistAction' and there is no
-- way to detect that. There are various alternatives to this but I
-- haven’t yet decided which one is best.
          | model /= model' ->
            Effect
              model'
              ((OtherAction persistAction <$
                setLocalStorage "miso.persist.model" model') :
               map (fmap OtherAction) acts)
          | otherwise -> Effect model' (map (fmap OtherAction) acts)
    view' = \model -> fmap OtherAction (view model)
    subs' = map (mapSubAction OtherAction) subs
