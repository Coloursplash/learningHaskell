mean :: Floating a => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

variance :: Floating a => [a] -> a
variance xs = sum (map meanSquare xs) / fromIntegral (length xs)
  where
    meanSquare x = (x - m)^2
    m = mean xs

standardDeviation :: Floating a => [a] -> a
standardDeviation = sqrt . variance

main :: IO()
main = do
  print ( mean [1, 5, 4, 2,6 ,7 ,8 ,8, 6] )
  print ( variance [1, 5, 4, 2,6 ,7 ,8 ,8, 6] )
  print ( standardDeviation [1, 5, 4, 2,6 ,7 ,8 ,8, 6] )
