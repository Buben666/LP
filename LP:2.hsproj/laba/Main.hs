import Data.List
import Data.Maybe

vi :: [Char] -> [Int] -> String -> String
vi z x = zipWith c (cycle x).mapMaybe (`elemIndex` z)
  where leng = length z 
        c v b = z!!((v+b) `mod` leng)









