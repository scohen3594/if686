-- TRABALHO 3

--questão 1

type Table = [(Int, Int)]

ex :: Table
ex = [(1,2), (2,4), (3,9)]

-- O(n)
get :: Table -> Int -> (Int, Int)
get [] key = (key, 0)
get tabela key | hasKey tabela key == False = (key, 0)
			   | otherwise = head ([x | x <- tabela, (fst x == key)])

-- O(n) - quando a key ja existe, ele chama put com a proxima key (key+1)
put :: Table -> (Int,Int) -> Table 
put table pair | (fst pair) < fst (head table) = [pair] ++ table
			   | (fst pair) == fst (head table) = (head table):(put (tail table) ((fst pair)+1, snd pair))
			   | otherwise = (head table):(put (tail table) pair)

-- O(n)
remove :: Table -> Int -> Table
remove [] element = []
remove table element = [x | x <- table, (snd x /= element)]

-- O(n)
hasKey :: Table -> Int -> Bool
hasKey tabela key = length ([x | x <- tabela, (fst x == key)]) == 1

-- questão 2
comparaConjuntos :: [Int] -> [Int] -> String
comparaConjuntos a b
 | a == [] && b == [] = "Conjuntos vazios"
 | a == [] = "B contem A"
 | b == []  = "A contem B" 
 | (teste a b) == [] = "Conjuntos disjuntos"
 | length (teste a b) == length (organizar (qs b)) && length (organizar (qs a)) == length (organizar (qs b)) = "Conjuntos iguais"
 | length (teste a b) == length (organizar (qs b)) && length (organizar (qs a)) > length (organizar (qs b)) = "A contem B"
 | length (teste a b) == length (organizar (qs a)) && length (organizar (qs a)) < length (organizar (qs b)) = "B contem A"
 | length (teste a b) < length (organizar (qs a)) && length (teste a b) < length (organizar (qs b)) = "A interseciona B"
 
teste :: [Int] -> [Int] -> [Int] 
teste a b = subconj (organizar (qs a)) (organizar (qs b)) 

subconj :: [Int] -> [Int] -> [Int]
subconj a b = [x| x <- a, (member b x)]

organizar :: [Int] -> [Int]
organizar (a:as)
 | as == [] = [a]
 | a /= (head as) = (++) [a] (organizar as)   
 | otherwise = organizar as

qs :: [Int] -> [Int] 
qs [] = []
qs (a:as) = (qs [x|x<-as, x<=a]) ++ (a:qs[y|y<-as, y > a])

member :: [Int] -> Int -> Bool
member lista a  = [x|x <- lista, (x==a)] /= []




-- EXERCICIOS DA AULA

take1 :: [t] -> Int -> [t]
take1 [] n = []
take1 (a:as) n 
 | n > 0 = (++) [a] (take1 as (n-1)) 
 | otherwise = []


drop1 :: [t] -> Int -> [t]
drop1 [] n = []
drop1 (a:as) n
 | n > 0 = drop1 as (n-1)
 | otherwise = (a:as)


takeWhile1 :: (t -> Bool) -> [t] -> [t]
takeWhile1 fun [] = []
takeWhile1 fun (a:as)
 | fun a == True = (++) [a] (takeWhile1 fun as) 
 | otherwise = []


dropWhile1 :: (t -> Bool) -> [t] -> [t]
dropWhile1 fun [] = []
dropWhile1 fun (a:as)
 | fun a == True = dropWhile1 fun as
 | otherwise = (a:as)


--quicksort
qs1 :: Ord t => [t] -> [t] 
qs1 [] = []
qs1 (a:as) = (qs1 [x|x<-as, x<=a]) ++ (a:qs1[y|y<-as, y > a])


--agrupar -- dá erro ao tentar generalizar
separar :: String -> [Char] --separa primeiro elemento de uma string
separar (a:as) = [a]

teste1 :: String -> [[Char]] --concatena elemento por elemento da string
teste1 a
 | a == [] = []
 | otherwise = (separar a):(teste1 (tail a))

juntar :: [String] -> [[Char]] --juntar todas as strings desmembradas em uma só lista
juntar (a:as) 
 | as == [] = (teste1 a)
 | otherwise = (teste1 a)++(juntar as)

contar :: String -> [String] -> Int --conta dado elemento em uma lista
contar s lista = length [x|x <- lista, (x==s)]

tirar :: [String] -> String -> [String]
tirar [] n = []
tirar lista n = [x|x <- lista, (x /= n)]

agrupar :: [String] -> [([Char], Int)] --agrupa cada string com a quantidade de vezes que ela aparece
agrupar lista
 | lista == [] = []
 | otherwise = ([((head j), (contar (head j) j))]) ++ (agrupar (tirar j (head j)))
 	where
 		j = (juntar lista)
