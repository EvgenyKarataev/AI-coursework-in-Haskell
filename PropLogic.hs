----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------
--                                                                                          --
-- � ������ PropLogi� ����������� ������� ��� ������ � ����������� ��������������.          --
--                                                                                          --
-- �����: �������� �������                                                                  --
-- ����:  01.11.10                                                                          --
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------
 
module PropLogi�
	
  (allEnvs,  eval, showPropStat, transform, insertNeg, vars, tautology)
	
where

----------------------------------------------------------------------------------------------
-- ���������� ��������� ������:                                                             --
--            List - ����� ������� ������� nub.                                             --

import List
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- ��������� ������������ ��������� ������                                                  --

type Name = Char

-- ��������� ���, ���������� ������ �������� ����������
-- �������� � ���� ('a', True)
type Environment c d = [(c, d)]

-- PropStat - ������������ ������������ 
data PropStat = Simple Name
		| And     PropStat PropStat                 -- A && B
		| Or      PropStat PropStat                 -- A || B
		| Not     PropStat                          -- not A
		| Implies PropStat PropStat                 -- A -> B
		| Equiv   PropStat PropStat                 -- A == B
		deriving Show

-- �������� ������ Eq ��������� ��������� �� ��������� ��� ��������� PropStat
instance Eq PropStat where
   p1 == p2 = checkTwoProp p1 (allEnvs (vars p1)) p2 (allEnvs (vars p2))
              where checkTwoProp p1 (h1: t1) p2 (h2: t2) | eval p1 h1 == eval p2 h2 = checkTwoProp p1 (t1) p2 (t2)
				     | otherwise = False 

                    checkTwoProp p1 [] p2 [] = True
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- ��������������� �������, ��� ������������.
-- �� ����� �� 1 �� 4 ���������� �������� �������������.

stat :: Int -> PropStat
stat 1 = Equiv (Simple 'c') (Simple 'd')
stat 2 = Implies (Simple 'a') (Or (Simple 'a') (Simple 'b'))
stat 3 = Equiv (Implies (Simple 'c') (Simple 'd')) (Implies (Simple 'd') (Simple 'c'))
stat 4 = Equiv (Not (And (Not (Simple 'a')) (Not (Simple 'c')))) (Implies (Not (Simple 'c')) (Simple 'a'))
----------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------- 
-- allEnvs, ���������� ������ ������� ���� ��������� ������������� ���������� �� ������.
-- ������� ���������:
--          � - ������ ����������.
-- ������������ ��������:
--          ������ ������� ���� ��������� �������� ����������.
--
-- ����������:
--          allLines, ���������� ������ ���� ������������� ���������� � ����������� �� ���������� ����.
--          ������� ���������:
--                   n - ���������� ��������� ������� ����� ���������� (����� 2 ^ (���������� ����������)).
--                   s - ����� ����.
--                   � - ������ ����������.
--          
--          powerOfTwo m, ���� m > 0 ���������� 2 ^ m, ����� ���������� 0.
--
--          getLine, ���������� ���� ������������� ����������
--          ������� ���������:
--                   n - ���������� ��������� ���������� �������� ��� ������� ���������.
--                   s - ���.
--                   i - ����� ������� ����������.
--                   (h:t) - ������ ���������� ���������� �� ������ ����.

allEnvs :: [Name] -> [Environment Name Bool]
allEnvs [] = []
allEnvs a = allLines (length a) 1 a
      where allLines n s a | s <= powerOfTwo n = genLine (powerOfTwo (n - 1)) s 1 a : allLines n (s + 1) a
		           | otherwise = []
       
	    powerOfTwo m | m >= 0 = 2 ^ m
	                 | otherwise = 0

            genLine n s i (h:t) | n < 1 = []
			    | s <= n = (h, False) : genLine (n / 2) (s) (i+2) (t)
			    | s > n = (h, True) : genLine (n / 2) (s - n) (i + 2) (t)
			    | otherwise = []
            genLine n s i [] = []
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- Eval, ����� ������������ p � ������������� e, � ��������� p � ������������� e.
-- ������� ���������:
--          � - ������������.
--          � - ������� �������� ����������.
-- ������������ ��������:
--          True - ���� ������������ �������, ����� False. 

eval :: PropStat -> Environment Name Bool -> Bool
eval (Simple a) ((c,d):t) | a == c = d
			  | otherwise = eval (Simple a) (t)

eval (Simple a) [] = False
eval (And a b) env = (eval a env) && (eval b env)
eval (Or a b) env = (eval a env) || (eval b env)
eval (Not a) env = not (eval a env)
eval (Implies a b) env = not (eval a env) || (eval b env)
eval (Equiv  a b) env = (eval a env) == (eval b env)
--eval (Simple a) [] = error "variable" ++ show a ++ "not found"
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- showPropStat, ����� �������- ����� � ����������  ������,  �������������� ��� ������������.  
-- ����������  ���  ������������ � ������.
-- ������� ���������:
--          � - ������������.
-- ������������ ��������:
--          ������, �������������� ��� ������������.

showPropStat (Simple a) = [a]
showPropStat (And a b) = "(" ++ (showPropStat a) ++ " && " ++ (showPropStat b) ++ ")"
showPropStat (Or a b) = "(" ++ (showPropStat a) ++ " || " ++ (showPropStat  b) ++ ")"
showPropStat (Not a) = "(" ++ "not " ++  (showPropStat a) ++ ")"
showPropStat (Implies a b) = "(" ++ (showPropStat a) ++ " -> " ++ (showPropStat  b) ++ ")"
showPropStat (Equiv a b) = "(" ++ (showPropStat  a) ++ " == " ++ (showPropStat  b) ++ ")"
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- transform, ������������ ������������ � ������������� (->, not)������.
-- ������� ���������:
--          � - ������������.
-- ������������ ��������: 
--          �������������  ������������ � (->, not)������.

transform (Simple a) = Simple a
transform (Not a) = Not (transform a)
transform (Implies a b) = Implies (transform a) (transform b)
transform (And a b) = Not (transform (Or (Not (transform a)) (Not (transform b))))
transform (Or a b) = Implies (Not (transform a)) (transform b)
transform (Equiv a b) = transform (And (Implies (transform a) (transform b)) (Implies (transform b) (transform a)))
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- insertNeg, �������� �������  � ����  � ������� �����������.
-- insertNeg � ������ ���������� �������, ������������ �� ���������� ������� � ��������� 
-- ���� ���������� ��������� ������ ���������� � ����������.
-- ������� ���������:
--          � - ������������.
-- ������������ ��������:
--          ������� � ���� � ������� �����������.

insertNeg (Simple a) = Simple a
insertNeg (Not (And b c)) = Or (insertNeg (Not b)) (insertNeg (Not c))
insertNeg (Not (Or b c)) = And (insertNeg (Not b)) (insertNeg (Not c))
insertNeg (Not (Not a)) = a
insertNeg (Not a) = Not (insertNeg a)
insertNeg (Implies a b) = Implies (insertNeg  a) (insertNeg  b)
insertNeg (And a b) = And (insertNeg a) (insertNeg b)
insertNeg (Or a b) = Or (insertNeg  a) (insertNeg b)
insertNeg (Equiv a b) = Equiv (insertNeg a) (insertNeg b)
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- vars, ���������� ������ ���� ����������  ���  ����������,  ������������  �  ������������.
-- ������� ���������:
--          � - ������������.
-- ������������ ��������:
--          ������ ���� ����������, ������������ � ������������.

vars a = nub (allvars a)
     where allvars (Simple a) = a : []
           allvars (And a b) = (allvars a) ++ (allvars b) 
           allvars (Or a b) = (allvars a) ++ (allvars b)
           allvars (Not a) = allvars a 
           allvars (Implies a b) = (allvars a) ++ (allvars b)
           allvars (Equiv a b) = (allvars a) ++ (allvars b)
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- tautology, ��������� �������� �� ������������ �����������.
-- ������� ���������:
--          � - ������������.
-- ������������ ��������:
--          True - ���� ������������ ����������, ����� False.

tautology prop = checkEachCase prop (allEnvs (vars prop))
       where checkEachCase prop (h: t) | eval prop h = checkEachCase prop (t)
                        | otherwise = False
             checkEachCase prop [] = True;
----------------------------------------------------------------------------------------------