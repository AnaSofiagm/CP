module Problema2 where

import BTree
import Cp
import List
import Data.List
import Data.Monoid
import System.Process
import GHC.IO.Exception
import Exp


emp x = Node(x,(Empty,Empty))

t7 = emp 7
t16 = emp 16
t7_10_16 = Node(10,(t7,t16))
t1_2_nil = Node(2,(emp 1, Empty)) 
t1 = Node(5,(t1_2_nil, t7_10_16))

t0_2_1 = Node(2, (emp 0, emp 3))
t5_6_8 = Node(6, (emp 5, emp 8))
t2 = Node(4, (t0_2_1, t5_6_8))

dotBt :: (Show a) => BTree a -> IO ExitCode
dotBt = dotpict . bmap Just Just . cBTree2Exp . (fmap show)


{-}
maisDir :: BTree a -> Maybe a
maisDir Empty = Nothing
maisDir (Node (a, (e, d))) = cond (isEmpty . p2 . p2) (Just . p1) (maisDir . p2 . p2) (a,(e,d))


isEmpty::BTree a -> Bool
isEmpty Empty = True
isEmpty _ = False
-}

maisDir :: Eq a => BTree a -> Maybe a
maisDir = cataBTree g where g = either (const Nothing) (cond ((==Nothing) . p2 . p2) (Just . p1) (p2 . p2))

{-}
f::Eq a => (a, (Maybe a, Maybe a)) -> Maybe a
f = cond ((==Nothing) . p1 . p2) (Just . p1) (p1 . p2)
-}

maisEsq :: Eq a => BTree a -> Maybe a
maisEsq = cataBTree g where g = either (const Nothing) (cond ((==Nothing) . p1 . p2) (Just . p1) (p1 . p2))


{-}
insOrd::(Ord a) => a -> BTree a -> BTree a
insOrd a Empty = Node (a,(Empty,Empty))
insOrd a (Node (b,(e,d))) = if a==b then (Node(b,(e,d))) else (if (a>b) then (Node(b,(e,insOrd a d))) else (Node(b,(insOrd a e,d))))
-}

insOrd::(Ord a) => a -> BTree a -> BTree a
insOrd a = p1 . (insOrd' a)



insOrd'::(Ord a) => a -> BTree a -> (BTree a, BTree a)
insOrd' x = cataBTree g where g = split (either (const (Node (x,(Empty,Empty)))) (cond ((==x) . p1) igual (cond ((>x) . p1) menor maior))) (either (const Empty) igual)

--  ==x
igual::(a,((BTree a,BTree a),(BTree a,BTree a))) -> BTree a
igual = (Node . (split p1 (split (p2 . p1 . p2) (p2 . p2 . p2))))

-- <x
maior::(a,((BTree a,BTree a),(BTree a,BTree a))) -> BTree a
maior = (Node . (split p1 (split (p2 . p1 . p2) (p1 . p2 . p2))))

-- >x
menor::(a,((BTree a,BTree a),(BTree a,BTree a))) -> BTree a
menor = (Node . (split p1 (split (p1 . p1 . p2) (p2 . p2 . p2))))


isOrd::(Ord a) => BTree a -> Bool
isOrd = p1 . isOrd'


isOrd'::(Ord a) => BTree a -> (Bool, BTree a)
isOrd' = cataBTree g where g = split (either (const True) h) (either (const Empty) (Node . (split p1 (split (p2 . p1 . p2) (p2 . p2 . p2)))))




f :: (Ord a) => (a -> a -> Bool) -> (a, BTree a) -> Bool
f _ (_,Empty) = True
f b (a,(Node (c,(e,d)))) = b a c

{-
h :: (Ord a) => (a,((Bool,BTree a),(Bool,BTree a))) -> Bool
h = (uncurry (&&)) . (split ((f (<)) . (split p1 (p2 . p1 . p2))) ((f (>)) . (split p1 (p2 . p2 . p2))))
-}

h :: (Ord a) => (a,((Bool,BTree a),(Bool,BTree a))) -> Bool
h = a . (split (a . split (p1 . p1 . p2) ((f (>)) . (split p1 (p2 . p1 . p2)))) (a . split (p1 . p2 . p2) ((f (<)) . (split p1 (p2 . p2 . p2)))))
                                                      where a=uncurry (&&)



rrot :: BTree a -> BTree a
rrot (Node(a,(Node(e,(e1,d1)),d))) = Node(e,(e1,Node(a,(d1,d)))) 
rrot a = a


lrot :: BTree a -> BTree a
lrot (Node(a,(e,Node(d,(e1,d1))))) = Node(d,(Node(a,(e,e1)),d1)) 
lrot a = a


rnode :: BTree a -> BTree a
rnode Empty = Empty
rnode (Node (a,(e,d))) = d

lnode :: BTree a -> BTree a
lnode Empty = Empty
lnode (Node (a,(e,d))) = e


splay :: [Bool] -> (BTree a -> BTree a)
splay = cataList g where g  = either (const idb) ((uncurry (.)) . (id >< l) . swap)


l x = if x then lnode else rnode


--rt :: (Bool,(BTree a -> BTree a)) -> (BTree a -> BTree a)
rt a = if a then (. rnode) else (. lnode)


idb :: BTree a -> BTree a
idb a = a

--splay :: [Bool] -> BTree a -> BTree a
--splay a b = (cataList g a) b where g  = either (const Empty) (l)