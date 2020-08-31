data Perceptron a b = Perceptron [a] b

weights :: Perceptron a b -> [a]
weights (Perceptron w b) = w

bias :: Perceptron a b -> b
bias (Perceptron w b) = b

predict :: (Ord a, Num a, Num b) => Perceptron a a -> [a] -> c -> b
predict (Perceptron w b) f l = if (dot w f) > b then 1 else 0

dot :: Num a => [a] -> [a] -> a 
dot [] [] = 0
dot (x:xs) (y:ys) = x * y + dot xs ys

initPerceptron :: (Num a, Num b) => Int -> Perceptron a b
initPerceptron featureLength = Perceptron (replicate featureLength 0) 0
 
