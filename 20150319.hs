double :: [Int] -> [Int]
double lista 
 | lista == [] = []
 | otherwise = (++) [(head lista) * 2] (double (tail lista))

--outra forma (casamento de padrões)
double2 [] = []
double2 (h:t) = (2*h):(double2 t)




member :: [Int] -> Int -> Bool
member list a
 | list == [] = False
 | head list == a = True
 | otherwise = (member (tail list) a)

--outra forma (casamento de padrões)
member2 [] a = False
member2 (h:t) a = (h==a) || (member2 t a) 




digits :: String -> String
digits cadeia
 | cadeia == [] = []
 | (head cadeia >= '0' && head cadeia <= '9') = (++) [(head cadeia)] (digits (tail cadeia))
 | otherwise = digits (tail cadeia)

--outra forma (casamento de padrões)




sumPairs :: [Int] -> [Int] -> [Int]
sumPairs lista1 lista2
 | lista1 == [] && lista2 == [] = []
 | lista1 == [] = lista2
 | lista2 == [] = lista1
 | otherwise = (++) [(head lista1 + head lista2)] (sumPairs (tail lista1) (tail lista2))

--outra forma (casamento de padrões)
sumPairs2 [] [] = []
sumPairs2 [] (h:t) = (h:t)
sumPairs2 (h:t) [] = (h:t)
sumPairs2 (h1:t1) (h2:t2) = (h1+h2):(sumPairs2 t1 t2)