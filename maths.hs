inc :: Num a => a -> a
inc x = x + 1

add :: Num a => a -> a -> a
add x y = x + y

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (a:as) = a + sumList as

fact :: Int -> Int
fact n
  | n < 0 = -1
  | n == 0 = 1
  | otherwise = n * (fact (n - 1))

fib :: Int -> Int
fib n
  | n < 0 = -1
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = (fib (n - 2)) + (fib (n - 1))

max :: Ord a => a -> a -> a
max a b
  | a > b = a
  | otherwise = b

min :: Ord a => a -> a -> a
min a b
  | a < b = a
  | otherwise = b

main :: IO()
main = do
  print (inc 3)
  print (add 4 9)
  print (sumList [3, 5, 4, 8, 7, 9, 3, 5, 2, 1, 1])
  print (fact 4)
  print (fib 15)
