----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------
--                                                                                          --
-- В модуле PropLogiс реализованы функции для работы с логическими высказываниями.          --
--                                                                                          --
-- Автор: Каратаев Евгений                                                                  --
-- Дата:  01.11.10                                                                          --
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------
 
module PropLogiс
	
  (allEnvs,  eval, showPropStat, transform, insertNeg, vars, tautology)
	
where

----------------------------------------------------------------------------------------------
-- Подгружаем некоторые модули:                                                             --
--            List - чтобы вызвать функцию nub.                                             --

import List
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- Описываем используемые структуры данных                                                  --

type Name = Char

-- Описывает тип, позволящий задать значение переменной
-- Например в виде ('a', True)
type Environment c d = [(c, d)]

-- PropStat - представляет высказывание 
data PropStat = Simple Name
		| And     PropStat PropStat                 -- A && B
		| Or      PropStat PropStat                 -- A || B
		| Not     PropStat                          -- not A
		| Implies PropStat PropStat                 -- A -> B
		| Equiv   PropStat PropStat                 -- A == B
		deriving Show

-- Экземляр класса Eq позволяет оценивать на равенство два выражения PropStat
instance Eq PropStat where
   p1 == p2 = checkTwoProp p1 (allEnvs (vars p1)) p2 (allEnvs (vars p2))
              where checkTwoProp p1 (h1: t1) p2 (h2: t2) | eval p1 h1 == eval p2 h2 = checkTwoProp p1 (t1) p2 (t2)
				     | otherwise = False 

                    checkTwoProp p1 [] p2 [] = True
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- Вспомогательная функция, для тестирования.
-- По числу от 1 до 4 возвращает заданное высказываение.

stat :: Int -> PropStat
stat 1 = Equiv (Simple 'c') (Simple 'd')
stat 2 = Implies (Simple 'a') (Or (Simple 'a') (Simple 'b'))
stat 3 = Equiv (Implies (Simple 'c') (Simple 'd')) (Implies (Simple 'd') (Simple 'c'))
stat 4 = Equiv (Not (And (Not (Simple 'a')) (Not (Simple 'c')))) (Implies (Not (Simple 'c')) (Simple 'a'))
----------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------- 
-- allEnvs, возвращает список списков всех возможных интерпретаций переменных из списка.
-- Входные параметры:
--          а - список переменных.
-- Возвращаемое значение:
--          Список списков всех возможных значений переменных.
--
-- Подфункции:
--          allLines, возвращает список всех интерпретаций переменных в зависимости от начального шага.
--          Входные параметры:
--                   n - количество возможных зачений одной переменной (равно 2 ^ (количество переменных)).
--                   s - номер шага.
--                   а - список переменных.
--          
--          powerOfTwo m, если m > 0 возвращает 2 ^ m, иначе возвращает 0.
--
--          getLine, возвращает одну интерпритацию переменных
--          Входные параметры:
--                   n - количество возможных одинаковых значений для текущей перменной.
--                   s - шаг.
--                   i - номер текущей переменной.
--                   (h:t) - список оставшихся переменных на данном шаге.

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
-- Eval, берет высказывание p и интерпретацию e, и оценивает p в интерпретации e.
-- Входные параметры:
--          р - высказывание.
--          е - спиской значений переменных.
-- Возвращаемое значение:
--          True - если высказывание истинно, иначе False. 

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
-- showPropStat, берет высказы- вание и возвращает  строку,  представляющую это высказывание.  
-- Заключайте  все  подвыражения в скобки.
-- Входные параметры:
--          р - высказывание.
-- Возвращаемое значение:
--          Строка, представляющая это высказывание.

showPropStat (Simple a) = [a]
showPropStat (And a b) = "(" ++ (showPropStat a) ++ " && " ++ (showPropStat b) ++ ")"
showPropStat (Or a b) = "(" ++ (showPropStat a) ++ " || " ++ (showPropStat  b) ++ ")"
showPropStat (Not a) = "(" ++ "not " ++  (showPropStat a) ++ ")"
showPropStat (Implies a b) = "(" ++ (showPropStat a) ++ " -> " ++ (showPropStat  b) ++ ")"
showPropStat (Equiv a b) = "(" ++ (showPropStat  a) ++ " == " ++ (showPropStat  b) ++ ")"
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- transform, переписывает высказывание в эквивалентную (->, not)–форму.
-- Входные параметры:
--          р - высказывание.
-- Возвращаемое значение: 
--          эквивалентное  высказывание в (->, not)–форме.

transform (Simple a) = Simple a
transform (Not a) = Not (transform a)
transform (Implies a b) = Implies (transform a) (transform b)
transform (And a b) = Not (transform (Or (Not (transform a)) (Not (transform b))))
transform (Or a b) = Implies (Not (transform a)) (transform b)
transform (Equiv a b) = transform (And (Implies (transform a) (transform b)) (Implies (transform b) (transform a)))
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- insertNeg, приводит формулу  к виду  с тесными отрицаниями.
-- insertNeg р выдает логическую формулу, получающуюся из логической формулы р внесением 
-- всех операторов отрицания внутрь конъюнкций и дизъюнкций.
-- Входные параметры:
--          р - высказывание.
-- Возвращаемое значение:
--          Формула в виде с тесными отрицаниями.

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
-- vars, возвращает список всех переменных  без  дупликатов,  содержащихся  в  высказывании.
-- Входные параметры:
--          а - высказывание.
-- Возвращаемое значение:
--          Список всех переменных, содержащихся в высказывании.

vars a = nub (allvars a)
     where allvars (Simple a) = a : []
           allvars (And a b) = (allvars a) ++ (allvars b) 
           allvars (Or a b) = (allvars a) ++ (allvars b)
           allvars (Not a) = allvars a 
           allvars (Implies a b) = (allvars a) ++ (allvars b)
           allvars (Equiv a b) = (allvars a) ++ (allvars b)
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- tautology, проверяет является ли высказывание тавтологией.
-- Входные параметры:
--          р - высказывание.
-- Возвращаемое значение:
--          True - если высказывание тавтология, иначе False.

tautology prop = checkEachCase prop (allEnvs (vars prop))
       where checkEachCase prop (h: t) | eval prop h = checkEachCase prop (t)
                        | otherwise = False
             checkEachCase prop [] = True;
----------------------------------------------------------------------------------------------