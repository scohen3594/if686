vendas :: Int -> Int
vendas val = val*2

fun :: Int -> Int -> Int
fun n val
 | n == 0 && vendas n == val = n
 | n == 0 = 0
 | vendas n == val = (fun (n-1) val) + 1
 | otherwise = (fun (n-1) val)