isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0
main = do
    print (isEven 2)