kek :: Integer -> Integer -> (Integer, Integer);
  kek x y | (abs y > abs x) && ((y > 0 && x > 0) || (y > 0 && x < 0)) = (0,x)
          | (abs y > abs x) && ((y < 0 && x < 0) || (y < 0 && x > 0)) = (0,(-x))
          | (y > 0) && (x > 0) = let (r, d) = kek (x-y) y
                                     in (1+r, d)  
          | ((y < 0) && (x > 0)) || ((y > 0) && (x < 0)) = let (r, d) = kek (x+y) y
                                                               in (1+r, d)
          | (y < 0) && (x < 0) = let (r, d) = kek (x-y) y
                                     in (1+r, d)
          | (x == 0) && ((y > 0) || (y < 0)) = (0, 0)
          | otherwise = error "На ноль делить нельзя!!!"
