module Miso.Middleware.Internal.Tree
  ( RoseZipper(..)
  , RoseTree(..)
  , moveUp
  , moveDown
  , addChild
  , insertAndMoveTo
  ) where

import Data.Maybe

data RoseTree a = RoseTree a [RoseTree a] deriving (Show, Eq, Ord)

data RoseZipper a =
  RoseZipper (RoseTree a)
       [([RoseTree a], a, [RoseTree a])]
  deriving (Show, Eq, Ord)

moveUp :: RoseZipper a -> Maybe (RoseZipper a)
moveUp (RoseZipper _ []) = Nothing
moveUp (RoseZipper n ((left, v, right):ps)) =
  Just (RoseZipper (RoseTree v (left ++ [n] ++ right)) ps)

splitAt' :: Int -> [a] -> Maybe ([a], a, [a])
splitAt' _ [] = Nothing
splitAt' 0 (x:xs) = Just ([], x, xs)
splitAt' i (x:xs) = do
  (ls, x, rs) <- splitAt' (i - 1) xs
  pure (x : ls, x, rs)

moveDown :: Int -> RoseZipper a -> Maybe (RoseZipper a)
moveDown i (RoseZipper (RoseTree v cs) ps) = do
  (ls, x, rs) <- splitAt' i cs
  pure (RoseZipper x ((ls, v, rs) : ps))

addChild :: RoseTree a -> RoseZipper a -> RoseZipper a
addChild c (RoseZipper (RoseTree v cs) ps) = RoseZipper (RoseTree v (c : cs)) ps

insertAndMoveTo :: RoseTree a -> RoseZipper a -> RoseZipper a
insertAndMoveTo c tree = fromJust (moveDown 0 (addChild c tree))
