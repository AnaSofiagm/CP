module Problema3 where

import Cp
import List
import LTree
import Data.List
import Data.Monoid
import System.Process
import GHC.IO.Exception
import St

data Bdt a = Dec a | Query (String, (Bdt a, Bdt a)) deriving Show

inBdt = either Dec Query

outBdt(Dec v) = i1 v
outBdt(Query (a,(s,n))) = i2(a,(s,n))


baseBdt f g h = f -|- g >< (h >< h) 
recBdt h = baseBdt id id h
cataBdt g = g . (recBdt (cataBdt g)) . outBdt
anaBdt g = inBdt . (recBdt (anaBdt g) ) . g


extLTree :: Bdt a -> LTree a
extLTree = cataBdt g where
                        g = either Leaf (Fork . p2)


--tipsBdt :: Bdt a -> [a]
--tipsBdt = cataBdt (either singl ((uncurry (++)) . p2))


[{-
navLTree :: LTree a -> ([Bool] -> LTree a)
navLTree a b = (cataLTree g) a
                    where g = 


k::Either () Bool -> (LTree a,LTree a) -> LTree a
k (Left ()) (e,d) = Fork (e,d)
k (Right b) (e,d) = if b then e else d


navLTree' :: LTree a -> ([Bool] -> LTree a)
navLTree' a b = (cataLTree g) a
                    where g = either Leaf f
                          f (e,d) = (either (const (Fork (e,d))) ((uncurry navLTree) . (cond (p1) (split (const e) p2) (split (const d) p2)))) (outList b)
-}


navLTree :: LTree a -> ([Bool] -> LTree a)
navLTree a b = (cataLTree g) a
                    where g = 

t=Fork(Fork(Fork (Leaf 1,Leaf 2),Fork (Leaf 3,Leaf 4)),Fork(Fork (Leaf 5,Leaf 6),Fork (Leaf 7,Leaf 8)))

deci :: [Bool] -> Either () (a,(LTree a,LTree a)) ->  Either () LTree a
deci [] a = a
deci _ (Left a) = Left a
deci (x:xs) (Right (a,(e,d))) = if x then  else d

