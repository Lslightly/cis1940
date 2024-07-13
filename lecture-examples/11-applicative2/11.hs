sequenceC :: Applicative f => [f a] -> f [a]
sequenceC = foldr g init 
    where g fa falist = (:) <$> fa <*> falist
          init = pure []

t = sequenceC [Just 1, Just 2]