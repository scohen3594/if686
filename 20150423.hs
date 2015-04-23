-- Slide 1

-- Fibonacci
fibFun :: Int -> [Int]
fibFun n
 | (n == 0) = []
 | otherwise = (fibFun (n-1)) ++ [fib (n*3)]

fib :: Int -> Int
fib n
 | n == 0 = 0
 | n == 1 = 1
 | otherwise = fib (n-1) + fib (n-2)

-- Ordenar lista
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (a:as) = (ordenar[x | x <- as, (soma (show x)) <= (soma (show a))]) ++ (a:(ordenar [x | x <- as, (soma (show x)) > (soma (show a))]))

soma :: String -> Int
soma s
 | s == [] = 0
 | otherwise = (read [(head s)]) + (soma (tail s)) 


-- Slide 2

type Word = String
type Line = [Word]

getWord1 :: String -> String
getWord1 s 
 | s == [] = []
 | otherwise = takeWhile (/= ' ') s

dropWord1 :: String -> String
dropWord1 s
 | s == [] = []
 | otherwise = dropWhile (/= ' ') s

dropSpace1 :: String -> String
dropSpace1 s
 | s == [] = []
 | otherwise =  drop 1 (dropWhile (/= ' ') s )

splitWords1 :: String -> [String]
splitWords1 [] = []
splitWords1 s 
 | (tail s) == [] = [[head s]]
 | otherwise = [getWord1 s] ++ (splitWords1 (dropSpace1 s))

getLine1 :: Int -> [Word] -> Line
getLine1 _ [] = []
getLine1 n lista
 | n == 0 = []
 | (length (head lista)) > n = []
 | otherwise = [(head lista)] ++ (getLine1 (n-(length (head lista))) (tail lista))

dropLine1 :: Int -> [Word] -> [Word]
dropLine1 _ [] = []
dropLine1 n lista
 | (length (head lista)) <= n = (dropLine1 (n-(length (head lista))) (tail lista))
 | otherwise = lista

splitLines1 :: [Word] -> [Line]
splitLines1 [] = []
splitLines1 lista = [getLine1 10 lista] ++ (splitLines1 (dropLine1 10 lista))

fill1 :: String -> [Line]
fill1 s = splitLines1 (splitWords1 s)

joinLines1 :: [Line] -> String
joinLines1 [] = ""
joinLines1 lista 
 | head lista == [] = []
 | otherwise = ((head (head lista)) ++ (joinLines1 [(tail (head lista))])) ++ (joinLines1 (tail lista)) 


-- Slide 3 -- Todos feitos

-- Slide 4

data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * (r*r)
area (Rectangle l1 l2) = l1*l2


data Dias = Domingo | Segunda Float [String] | Terca Float [String] | Quarta Float [String] | Quinta Float [String] | Sexta Float [String] | Sabado

-- (i)
fds :: Dias -> Bool
fds (Domingo) = True
fds (Sabado) = True
fds (_) = False

-- (ii)
plc :: Dias -> Bool
plc Sabado = False
plc Domingo = False
plc (Segunda n aulas) = verificar aulas
plc (Terca n aulas) = verificar aulas
plc (Quarta n aulas) = verificar aulas
plc (Quinta n aulas) = verificar aulas
plc (Sexta n aulas) = verificar aulas

verificar :: [String] -> Bool
verificar [] = False
verificar (a:as) = [x | x <- (a:as), (x=="plc")] /= []

-- Até aqui (do slide 4) foi feito em sala, só não tinha colocado no git


data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Show)

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add x1 x2) = (showExpr x1) ++ " + " ++ (showExpr x2)
showExpr (Sub x1 x2) = (showExpr x1) ++ " - " ++ (showExpr x2)


data List t = Nil | Cons t (List t) deriving (Eq, Show)

toList :: List t -> [t] 
toList Nil = []
toList (Cons a as) = a:(toList as)

fromList :: [t] -> List t 
fromList [] = Nil
fromList (a:as) = (Cons a (fromList as))


depth :: Tree t -> Int 
depth NilT = 0
depth (Node a t1 t2)
 | (1 + (depth t1)) > (1 + (depth t2)) = (1 + (depth t1))
 | otherwise = (1 + (depth t2))

collapse :: Tree t -> [t] 
collapse NilT = []
collapse (Node a t1 t2) = (collapse t1) ++ [a] ++ (collapse t2)

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ NilT = NilT
mapTree func (Node a t1 t2) = (Node (func a) (mapTree func t1) (mapTree func t2))

bfs :: (Ord t) => Tree t -> t -> Bool
bfs (NilT) _ = False
bfs (Node a t1 t2) n
 | (a == n) = True
 | (n < a) = (bfs t1 n)
 | otherwise = (bfs t2 n)



-- Slide 5

-- Exercício (2)

union :: Eq t => [t] -> [t] -> [t]
union l1 l2 = (foldr (:) [] l1) ++ (foldr (:) [] l2)

{-
*Defina uma função que, dada uma lista de Strings, a transforma em uma lista de números onde cada número dessa lista corresponde à soma dos “valores” dos caracteres do String que aparece na mesma posição da lista de entrada. Os valores correspondem à posição da letra no alfabeto ('a' = 1, 'b' = 2, 'c' = 3, etc.). A função ord (do pacote Data.Char) pode ser útil para resolver essa questão.
*Exercício de inserir em árvore e depois uma busca
-}

mapFilter :: (t->Bool)->[[t]]->[[t]]
mapFilter func [] = []
mapFilter func (a:as) = [x | x<-a, (func x)]:(mapFilter func as)

{-
*exercício da ordem inversa
-}

fstPair :: ([(t,t)] -> [t])
fstPair = (\x -> [y | (y,z) <- x])

bigger :: (Num t) => ([[t]] -> Int -> [[t]])
bigger = (\x y -> [z | z <- x, (length z) > y])

{-
*Dada uma lista de listas, criar uma lista que contém todos os elementos das sub-listas da lista de entrada, mas removendo duplicação
-}

mapFold :: (t -> u -> u) -> [u] -> [[t] -> u]
mapFold _ [] = []
mapFold func (a:as) = (\x -> (fold' func a x)):(mapFold func as)

fold' :: (t -> u -> u) -> u -> [t] -> u
fold' _ n [] = n
fold' func n (a:as) = func a (fold' func n as)

{-
*Exercício de aplicar uma lista a outra
-}


-- Slide 6

sum1 :: Int -> [Int] -> [Int]
sum1 c = map (+c)

{-Dada uma lista de números, obter o maior da lista-}
maxList :: (Ord a) => [a] -> a
maxList = maximum


-- Slide de monitoria dada em horário de aulas

--AFD
type Tran = (Int, Int)

--afd :: String -> [Int] -> [(Tran, Char)] -> Int -> [Int]
--afd = “111” [1, 2, 3] [((1, 1), '1'), ((1, 3), '0'), ((3, 2), '1')] 1 [2]

verificar :: String -> [Int] -> [(Tran, Char)] -> Int -> [Int] -> Bool
verificar s estados tr ini aceita
 | not(member1 estados ini) = False
 | (s==[]) && (member1 aceita ini) = True
 | (s==[]) && not(member1 aceita ini) = False
 | ((head s) == (snd (head tr))) && ((fst (fst (head tr))) == ini) = (verificar (tail s) tr (snd (fst(head tr))) aceita)
 | otherwise = verificar s (tail tr) ini aceita

member1 :: [Int] -> Int -> Bool
member1 lista n = [x|x <- lista, x==n] /= []

--