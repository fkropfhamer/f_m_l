data Perceptron a b = Perceptron {
      weights :: [a],
      bias :: b
    } deriving (Show)


predict :: (Ord a, Num a, Num b) => Perceptron a a -> [a] -> b
predict (Perceptron w b) f = if (dot w f) > b then 1 else 0

dot :: Num a => [a] -> [a] -> a 
dot [] [] = 0
dot (x:xs) (y:ys) = x * y + dot xs ys

init_perceptron :: (Num a, Num b) => Int -> Perceptron a b
init_perceptron featureLength = Perceptron (replicate featureLength 0) 0
 

update (Perceptron weights bias) features label = Perceptron (map (\x -> (fst x) - (snd x) * error) (zip weights features)) bias
  where error = calculate_error (Perceptron weights bias) features label  
          where calculate_error perceptron features label 
                 | prediction == label = 0
                 | prediction == 1 && label == 0 = 1
                 | otherwise = -1  
                   where prediction = predict perceptron features 


train perceptron feature_sets labels 0 = perceptron
train perceptron feature_sets labels epochs = train (train_iteration feature_sets labels) feature_sets labels (epochs - 1)


train_iteration perceptron [] _ = perceptron  
train_iteration perceptron (feature_set:feature_sets) (label:labels) = train_iteration (update perceptron feature_set label) feature_sets labels 

