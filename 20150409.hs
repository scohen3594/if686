import Data.Char

--Trabalho 6

-- Método 1
--Exercicio 1
data Arv1 t = Vazio1 | Arv1 ([t], [(t,t)]) [t] deriving (Eq, Show)

ar = Arv1 ([1,2,3,4,5], [(1,2), (1,3), (3,4), (3,5)]) [10, 10, 10, 10]

--Exercicio 2
fun :: Eq t => Arv1 t -> t -> Bool
fun (Vazio1) _ = False
fun (Arv1 (v, e) p) n = (rec (Arv1 (v, e) p) [n]) /= [] 

rec :: Eq t => Arv1 t -> [t] -> [t]
rec (Arv1 (_,_) _) [] = []
rec (Arv1 ([],_) _) _ = []
rec (Arv1 (v,e) p) (a:as)
 | [x| x <- v, (x==a)] == [] = rec (Arv1 (novoVert, e) p) as
 | otherwise = a : (rec (Arv1 (novoVert,e) p) (adjacente ++ as))
 	where
 		adjacente = [x| (x,y) <- e, (y==a)] ++ [x| (y,x) <- e, (y==a)]
 		novoVert = [x| x <- v, (x /= a)]


-- Método 2
--Exercicio 1
data Arv t = Vazio | No t [(Int, Arv t)] deriving (Eq, Show)

grafo = (No 1 [(1, (No 3 [(1, (No 5 [(0, Vazio)])), (1, (No 6 [(0, Vazio)]))])), (1, (No 2 [(1, (No 4 [(0, Vazio)])), (1, (No 3 [(1, (No 5 [(0, Vazio)])), (1, (No 6 [(0, Vazio)]))]))])), (1, (No 7 [(0, Vazio)]))])

--Exercicio 2
dfs :: (Eq t) => Arv t -> t -> Bool 
dfs (No a list) n = theDfs n [((No a list))] [a]

theDfs :: (Eq t) => t -> [Arv t] -> [t] -> Bool 
theDfs n [] _ = False 
theDfs n ((No topo subs):stail) visited 
 | topo == n = True 
 | otherwise = (theDfs n ([ (No val lt) | (number, (No val lt)) <- subs, not (wasVisited val visited)] ++ stail) (topo:visited))

wasVisited :: (Eq t) => t -> [t] -> Bool 
wasVisited n visited = [ a | a<-visited, n==a] /= []





-- Exercicios aula

--(i)
map1 :: (t -> u) -> [t] -> [u]
map1 f [] = []
map1 f (a:as) = (f a):(map1 f as)

raiz :: Double -> Double
raiz n = sqrt n

calcular n = map1 raiz n


--(ii)
posicao :: Char -> Int
posicao x = ord x - ord 'a' + 1

posicaoAlfabeto s = map1 posicao s


--(iii)
map2 :: (t -> u) -> [t] -> [u]
map2 f (a:as) = [(f x)| x <- (a:as)]


--

fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:as) = f a (fold f as)

foldr' :: (t -> u -> u) -> u -> [t] -> u
foldr' f s [] = s
foldr' f s (a:as) = f a (foldr f s as)


-- Exercicios folding

--(1)
member :: Eq t => t -> [t] -> Bool
member s lista = foldr' (||) False (map (==s) lista) --[a==s| a <- lista]

--(2)
--union :: Eq t => [t] -> [t] -> [t]



