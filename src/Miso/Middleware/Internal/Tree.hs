{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Miso.Middleware.Internal.Tree
  ( RoseZipper(..)
  , RoseTree(..)
  , WithLocation(..)
  , Bounds(..)
  , moveUp
  , moveDown
  , addChild
  , insertAndMoveTo
  , design
  , height
  , treeBounds
  , makeAbsolute
  ) where

import Data.Maybe

data RoseTree a = RoseTree a [RoseTree a] deriving (Show, Eq, Ord, Functor)

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

-- Tree drawing: The algorithm is taken than from the functional pearl
-- “Drawing Trees”. If it is too slow,
-- <https://llimllib.github.io/pymag-trees/> presents a faster
-- algorithm but it’s harder to implement (especially in a functional
-- language).

data WithLocation a = WithLocation
  { val :: !a
  , location :: !Double
  } deriving (Show, Eq, Ord)

moveTree :: Double -> RoseTree (WithLocation a) -> RoseTree (WithLocation a)
moveTree offset (RoseTree (WithLocation v loc) cs) =
  RoseTree (WithLocation v (loc + offset)) cs

data Bounds = Bounds
  { leftBound :: !Double
  , rightBound :: !Double
  }

newtype Extent =
  Extent [Bounds]

moveExtent :: Double -> Extent -> Extent
moveExtent offset (Extent bounds) =
  Extent (map (\(Bounds l r) -> Bounds (l + offset) (r + offset)) bounds)

merge :: Extent -> Extent -> Extent
merge (Extent exts) (Extent exts') = Extent (merge' exts exts')
  where
    merge' [] qs = qs
    merge' ps [] = ps
    merge' (Bounds p _:ps) (Bounds _ q:qs) = Bounds p q : merge' ps qs

mergeList :: [Extent] -> Extent
mergeList = foldr merge (Extent [])

fit :: Extent -> Extent -> Double
fit (Extent (Bounds _ p:ps)) (Extent (Bounds q _:qs)) =
  max (fit (Extent ps) (Extent qs)) (p - q + 1)
fit _ _ = 0

fitListL :: [Extent] -> [Double]
fitListL = fitListL' (Extent [])
  where
    fitListL' acc [] = []
    fitListL' acc (e:es) =
      let x = fit acc e
      in x : fitListL' (merge acc (moveExtent x e)) es

flipExtent :: Extent -> Extent
flipExtent (Extent xs) = Extent (map (\(Bounds l r) -> Bounds (-l) (-r)) xs)

fitListR :: [Extent] -> [Double]
fitListR = reverse . map negate . fitListL . map flipExtent . reverse

mean :: Fractional a => a -> a -> a
mean x y = (x + y) / 2

fitList :: [Extent] -> [Double]
fitList es = zipWith mean (fitListL es) (fitListR es)

design :: RoseTree a -> RoseTree (WithLocation a)
design = fst . design'
  where
    design' (RoseTree label subtrees) =
      let (trees, extents) = unzip (map design' subtrees)
          positions = fitList extents
          pTrees = zipWith moveTree positions trees
          pExtents = zipWith moveExtent positions extents
          resultExtent =
            let Extent exts = mergeList pExtents
            in Extent ((Bounds 0 0) : exts)
          resultTree = RoseTree (WithLocation label 0) pTrees
      in (resultTree, resultExtent)

height :: RoseTree a -> Int
height (RoseTree _ []) = 1
height (RoseTree _ cs) = 1 + maximum (map height cs)

treeBounds :: RoseTree (WithLocation a) -> Bounds
treeBounds (RoseTree (WithLocation _ pos) cs) =
  let (ls, rs) = unzip (map ((\(Bounds l r) -> (l, r)) . treeBounds) cs)
  in Bounds (minimum (pos : ls)) (maximum (pos : rs))

makeAbsolute :: RoseTree (WithLocation a) -> RoseTree (WithLocation a)
makeAbsolute tree = makeAbsolute' 0 tree
  where
    makeAbsolute' !i (RoseTree (WithLocation v pos) cs) =
      RoseTree (WithLocation v (i + pos)) (map (makeAbsolute' (i + pos)) cs)
