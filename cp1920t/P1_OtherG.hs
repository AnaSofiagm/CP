module Problema1 where

import Exp
import Cp
import BTree
import LTree
import List
import Data.List
import FTree
import System.Process
import GHC.IO.Exception
import St



type Dict = Exp String String


---discollect--

discollect :: (Ord b, Ord a) => [(b,[a])] -> [(b,a)]
discollect a = (set . concat) [ [x |-> y2 | y2 <- y] | (x,y) <- a ]


---dic_imp --> importar dicionarios do formato "lista de pares palavra-traducoes"

dic_imp :: [(String, [String])] -> Dict
dic_imp = Term "" . map (bmap  id singl) . untar . discollect


---dic_norm --> normalizacao de um dicionario(forma de listas)

dic_norm = collect . filter p .discollect where
               p (a,b) = a>"" && b>""


---dic_red --> teste de redundancia de um significado s para uma palavra p

dic_red p s d = (p,s) `elem` discollect d

--f::(Ord v, Ord o) => [Exp v o] -> [([o],v)]
--f [] = []
--f ((Var v):t) = [([],v)] ++ (f t)
--f ((Term o l) : t) = ((map (k o) (f l)) ++ (f t))

{-
f::(Ord v, Ord o) => [Exp v o] -> [([o],v)]
f [] = []
f (h:t) = (g h) ++ (f t) where 
                           g (Var v) = [([],v)]
                           g (Term o l) = map (k o) (f l)
-}

k a= (ccat a) >< id

{-
s:: Exp v [o] -> [([o],v)]
s (Var v) = [([],v)]
s (Term o l) = map (k o) (f l) where f = concat . (map s)

dic_exp2 :: Dict -> [(String, [String])]
dic_exp2 = collect . s
-}
d =  [ ("ABA",["BRIM"]),
       ("ABALO",["SHOCK"]),
       ("AMIGO",["FRIEND"]),
       ("AMOR",["LOVE"]),
       ("MEDO",["FEAR"]),
       ("MUDO",["DUMB","MUTE"]),
       ("PE",["FOOT"]),
       ("PEDRA",["STONE"]),
       ("POBRE",["POOR"]),
       ("PODRE",["ROTTEN"])]


tar :: (Ord v, Ord o) => Exp v [o] -> [([o], v)]
tar = cataExp g where g = either (singl . (split nil id)) ((uncurry map) . (k >< concat))

dic_exp :: Dict -> [(String, [String])]
dic_exp = collect . tar



--procurar traducoes para uma determinada palavra

dic_rd2 :: String -> Dict -> Maybe [String]
dic_rd2 p t= lookup p (dic_exp t)

j::[String] -> Dict -> [String]
j [] (Var v) = [v]
j _ (Var v) = []
j [] (Term o l) = []
j (h:t) (Term o l)| o==h = (concat . map (j t)) l
                  | otherwise = []


h::[String] -> Maybe [String]
h [] = Nothing
h a = Just a 

s::String -> [String]
s a = "" : map singl a


dic_rd :: String -> Dict -> Maybe [String]
dic_rd = curry (h .uncurry (j . s))



--inserir palavras novas(palavra e traducao)
dic_in::String -> String -> Dict -> Dict
dic_in = (in_auxa . s) 


in_auxa::[String] -> String -> Dict -> Dict
in_auxa _ _ (Var v) = Var v
in_auxa _ "" d = d
in_auxa [] s d = Var s
in_auxa (h:t) s (Term o l) | o==h = if (dec_contem l t s) then Term o (map (in_auxa t s ) l) else Term o (l++(dic_cria t s))
                           | otherwise = (Term o l)
         

{-}
f::[Dict] -> [String] -> String -> [Dict]
f [] s1 s2= dic_cria s1 s2
f d [] s2 = map 
f ((Var v):t) s1 s2 = if v==s2 then ((Var v):t) else (Var v):(f t)
f ((Term o l):t) = if o==(head s1) then (in_auxa (Term o l)):t else (Term o l):(f t)
  -}

dec_contem::[Dict] -> [String] -> String -> Bool
dec_contem [] _ _ = False
dec_contem _ [] _ = True
dec_contem ((Var v):t) s1 s2 = if s2==v then True else (dec_contem t s1 s2)
dec_contem ((Term o l):t) s1 s2 = if (head s1)==o then True else (dec_contem t s1 s2)







dic_cria :: [String] -> String -> [Dict]
dic_cria [] v = [Var v]
dic_cria (h:t) v = [Term h (dic_cria t v)]



