--mergesort

merge :: [Int] -> [Int]
merge list 
 | list == [] = []
 | (tail list) == [] = list
 | otherwise = gather (merge (firstHalf list 0 ((div (length list) 2)-1))) (merge (secondHalf list 0 ((div (length list) 2)-1)))

firstHalf :: [Int] -> Int -> Int -> [Int] -- O(n)
firstHalf list at numero 
 | list ==[]=[]
 | at == numero = [head list]
 | otherwise = (head list) : (firstHalf (tail list) (at+1) numero)

secondHalf :: [Int] -> Int -> Int -> [Int] -- O(n)
secondHalf list at numero 
 | list == [] = []
 | at == numero = tail list
 | otherwise = (secondHalf (tail list) (at+1) numero)

gather :: [Int] -> [Int] -> [Int]
gather ar1 ar2 
 | ar1 == [] = ar2
 | ar2 == [] = ar1
 | (head ar1) <= (head ar2) = (head ar1):(gather (tail ar1) ar2)
 | otherwise = (head ar2):(gather ar1 (tail ar2))



--heapsort

swap :: Int -> Int -> Int -> Int -> Int -> [Int] -> [Int] --O(n)
swap x1 x2 p1 p2 i lista 
 | lista == [] = []
 | p1 == i = x2 : swap x1 x2 p1 p2 (i+1) (tail lista)
 | p2 == i = x1 : swap x1 x2 p1 p2 (i+1) (tail lista)
 | otherwise = (head lista) : (swap x1 x2 p1 p2 (i+1) (tail lista))

getValue :: [Int] -> Int -> Int -> Int --O(n)
getValue lista p1 i
 | p1 == i = (head lista)
 | otherwise = getValue (tail lista) p1 (i+1)

heapfy :: [Int] -> Int -> Int -> [Int]
heapfy lista node leng
 | lista == [] = []
 | (2*node+1) >= leng  = lista
 | (2*node+2) < leng && (getValue lista node 0) <= getValue (heapfy (heapfy lista (2*node+1) leng) (2*node+2) leng) (2*node+1) 0 && (getValue lista node 0) <= (getValue (heapfy (heapfy lista (2*node+1) leng) (2*node+2) leng) (2*node+2) 0) = (heapfy (heapfy lista (2*node+1) leng) (2*node+2) leng) 
 | (2*node + 2) < leng && (getValue lista node 0) > getValue (heapfy (heapfy lista (2*node + 1) leng) (2*node + 2) leng) (2*node + 1) 0	= heapfy (heapfy (swap (getValue lista node 0) (getValue (heapfy (heapfy lista (2*node + 1) leng) (2*node + 2) leng) (2*node + 1) 0) node (2*node + 1) 0 (heapfy (heapfy lista (2*node + 1) leng) (2*node + 2) leng)) (2*node + 1) leng) node leng
 | (2*node+2) < leng && (getValue lista node 0) > getValue (heapfy (heapfy lista (2*node+1) leng) (2*node+2) leng) (2*node+2) 0 = heapfy (heapfy (swap (getValue lista node 0) (getValue (heapfy (heapfy lista (2*node+1) leng) (2*node+2) leng) (2*node+2) 0) node (2*node+2) 0 (heapfy (heapfy lista (2*node+1) leng) (2*node+2) leng)) (2*node+2) leng) node leng
 | (getValue lista node 0) > getValue (heapfy lista (2*node+1) leng) (2*node+1) 0 = heapfy (heapfy (swap (getValue lista node 0) (getValue (heapfy lista (2*node+1) leng) (2*node+1) 0) node (2*node+1) 0 (heapfy lista (2*node+1) leng)) (2*node+1) leng) node leng
 | otherwise = lista

hsort :: [Int] -> [Int] --em média roda em O(n log n)
hsort lista
 | lista == [] = []
 | tail lista == [] = [head lista]
 | otherwise =  (++) [head (heapfy lista 0 (length lista))] (hsort (remove ((heapfy (swap (head (heapfy lista 0 (length lista))) (getValue (heapfy lista 0 (length lista)) ((length lista)-1) 0) 0 ((length lista)-1) 0 (heapfy lista 0 (length lista))) 0 ((length lista)-1)))))

remove :: [Int] -> [Int] --O(n)
remove lista
 | tail lista == [] = []
 | otherwise = (head lista):(remove (tail lista))