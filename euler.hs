module Euler where

import Data.List
import Data.Char
import Data.Maybe


----- EULER 12 ------- DONE
primes = sieve [2..]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primeFactors :: Integer -> [Integer]
primeFactors num = pf num primes

-- Pre: x > 1
pf :: Integer -> [Integer] -> [Integer]
pf x pr
          | x == prime          = [x]
          | x `mod` prime == 0  = prime : pf (x `div` prime) (primes) 
          | otherwise = pf x (tail pr)
              where prime = head(pr)

triangle x = sum [1..x]

triangleNums :: Integer -> [Integer]
triangleNums x = (triangle x) : (triangleNums (x+1))

tNums = triangleNums 2

numDivisors x = product $ map (+1) (map length $ group (primeFactors x))

euler12 = [tN| tN <- tNums, (numDivisors tN) > 500 ]

------ EULER 21 ------ DONE

sumDivisors :: Integer -> Integer
sumDivisors x | x == 1 = 0
              | otherwise = (sum $ nub $ map product $ subsequences (primeFactors x)) - x

isPrime x = length(primeFactors x) == 1

amicable x
            | sumDivisors (num) == x && num /= x = True
            | otherwise = False
                where   num = sumDivisors x


euler21 = sum [x | x <- [2..10000], amicable x]



----EULER 23 ------ DONE, but with help of python.......

abundant x = (sumDivisors x) > x
abundants = [x | x<- [1..28123], abundant x]

e23asdf = unfoldr help23 abundants

help23 [] = Nothing
help23 xs = Just(e23' xs, tail $ reverse xs)

e23' xs = map (+ (last xs)) xs


------ EULER 27 ------ DONE

isPrime2 x    | x <= 1 = False
              | x == 2 = True
              | x `mod` 2 == 0 = False
              | otherwise = isP x 3
isP x z
        | z^2 > x  = True
        | x `mod` z == 0 = False
        | otherwise = isP x (z+2)

quad a b x = x^2 + a*x + b

values a b = [quad a b x | x <- [1..b]]

primeValues a b = prv a b 0

prv a b x   | x == b || not (isPrime2 q) = []
            | otherwise = q : prv a b (x+1)
                where q = quad a b x

euler27 = maximum [ ((length $ primeValues a b),a,b) | a <-[(-999)..999], b<-[(-999)..999], (length $ primeValues a b) > 40]



---- EULER 28 -------   DONE


makeList delta x = [p | k <- [0..3], let p = x - k*delta ]
euler28 = (sum $ concatMap (\(x,y) -> (makeList y (x^2))) (map (\x -> ((2*x - 3),(2*x-4))) [3..502])) + 1



----- EULER 30 -------  DONE

toDigits x    | x < 10 = [x]
              | otherwise =  (x `mod` 10) : toDigits (x `div` 10)

tD x = reverse (toDigits x)

property5 x = (sum $ map (^5) li) == x
                  where li = tD x
                  
euler30 = sum [x | x<- [2..200000], property5 x]




---- EULER 31 ----- NOT DONE

euler31 = [(a,b,c,d,e,f,g) | 
            
            g <- [0..2],
            f <- [0..4],
            e <- [0..10],
            d <- [0..20],
            
            c <- [0..50],
            b <- [0..100],
            a <- [0..200],
            a*1 + b*2 + c*5 + d*10 + e*20 + f*50 + g*100 == 200]
            
            
e31 = [(a,b,c,d,e,f,g) | 

                        g <- [0..2],
                        f <- [0..(200-100*g `div` 50)],
                        e <- [0..(200-50*f `div` 20)],
                        d <- [0..(200-20*e `div` 10)],
                        c <- [0..(200-10*d `div` 5)],
                        b <- [0..(200-5*c `div` 2)],
                        a <- [(200 - (g*100 + f * 50 + e * 20 + d * 10 + c * 5 + b * 2))..(200-2*b)],
                        a*1 + b*2 + c*5 + d*10 + e*20 + f*50 + g*100 == 200]
            
            
            
            
            
            
            
------- EULER 32 ---------- DONE     
---Note: not optimzed at all

euler32 =    sum $ nub [ res | a <- [1..2000], b <- [1..200000], 
                let res = a*b, 
                let l = (tD a) ++ (tD b) ++ (tD res),
                isPanL l 9,
                length l == 9 ]
                    
                    
                    
---- EULER 33 ---------- DONE

multi [(a,x),(b,y),(c,z),(d,f)] = [(a*b*c*d,x*y*z*f)]

e33 = head $ multi [(a,b) |a <- [10..99], 
                b <- [10..99],
                a < b,
                let ad = tD a, 
                let bd = tD b,
                not (ad == reverse bd),
                let re = intersect ad bd,
                not (re == []),
                let res = head re,
                not (res == 0),
                let az = unmap (delete res ad),
                let bz = unmap (delete res bd),
                let k = (fromIntegral a) / (fromIntegral b),
                let r = (fromIntegral az) / (fromIntegral bz),
                k == r
                ]
                
                
euler33 = (snd e33) `div` (gcd (fst e33) (snd e33))
     
        

---- EULER 35 -------    DONE

rotations x = rot (tD x) 0

rot xs n
          | n == len - 1 = []
          | otherwise = li : rot li (n+1) 
             where  len = length xs
                    li = (tail xs) ++ [(head xs)]

unmap :: [Integer] -> Integer
unmap xs = umap xs (10^((length xs)-1))

umap :: [Integer] -> Integer -> Integer
umap xs n
            | xs == [] = 0
            | otherwise = ((head xs) * n) + (umap (tail xs) n `div` 10)
           
           
properRots x = map unmap $ rotations x

--Returns true if all rotations of x are prime 
rotsPrime x   = and $ map isPrime2 (properRots x)
            
circulars = [x | x <- [1..1000000], isPrime2 x, rotsPrime x]

euler35 = length circulars



---- EULER 36 --------- DONE

toBin x = toBin1 x []

toBin1 x a  | x == 0 = a
            | otherwise = toBin1 (x `div` 2) ( (x `mod` 2 ) : a)
            
isPalin x   | r == 1 = (take q l) == reverse (drop q $ tail l)
            | otherwise = (take q l) == reverse (drop q l)
              where   l = tD x
                      q = (length l) `div` 2
                      r = (length l) `mod` 2
                      
isPalinL l   | r == 1 = (take q l) == reverse (drop q $ tail l)
            | otherwise = (take q l) == reverse (drop q l)
                where 
                        q = (length l) `div` 2
                        r = (length l) `mod` 2


euler36 = [ res | res <- [1..1000000], isPalin res, isPalinL (toBin res)]



----- EULER 38 ------- DONE

cond num = cond1 num 1 []

cond1 num mult buf    | length buf > 9 = Nothing
                      | length buf == 9 && (isPanL buf 9) = Just buf
                      | otherwise = cond1 num (mult + 1) (buf ++ tD (num * mult))
                      
                      
euler38 = last $ sort $ [ p | x <- [1..10^4], let c = cond x, isJust c, let p = unmap $ fromJust (c)]


--- EULER 44 ------ DONE


pentN :: Integer -> Integer
pentN n = n*(3*n-1) `div` 2

--Code to check if x is pentagonal.

isPentagonal :: Integer -> Bool
isPentagonal x    | x < 1 = False
                  | otherwise = isInt (solution x)

solution :: Integer -> Float
solution x = ((0.5) + sqrt(0.25 + 6*(fromIntegral x))) / 3

isInt :: Float -> Bool
isInt x = x == fromInteger (round x)

euler44 = [ (abs(pentN a - pentN b)) | a <- [1..10000], b<- [1..10000], isPentagonal (pentN a + pentN b), isPentagonal (pentN a - pentN b)]





--- EULER 41 ---- DONE

isPandigital x n =  isPan (toList x) n 1

isPanL x n = isPan x n 1

isPan xs n a
                | a > n = True
                | a `elem` xs = isPan xs n (a+1) 
                | otherwise = False

pandigitals = map unmap $ permutations ([1,2,3,4,5,6,7])
                
euler41 = maximum [x | x <- pandigitals, isPrime2 x]


--- EULER 46 ---- NOT DONE







---- EULER 52 ------- DONE

same x y = a == b
              where a = sort $ tD x
                    b = sort $ tD y
                    
                    
euler52 = head [ a | a <- [1..], same (2*a) (3*a), same (3*a) (4*a), same (4*a) (5*a), same (5*a) (6*a)]




--- EULER 55 ------ DONE

op x = x + unmap(reverse p)
          where p = tD x

lychrel x = not (lych x 1)

lych num buf  | buf == 50 = False
              | isPalin $ op num = True
              | otherwise = lych ( op num ) (buf + 1)
              
euler55 = length [ p | p <- [1..10000], lychrel p]


---- EULER 87 -----   DONE

shortL = takeWhile (< 7070) primes

n = 50000000
l1 = takeWhile (< n) $ map (^2) shortL
l2 = takeWhile (< n) $ map (^3) shortL
l3 = takeWhile (< n) $ map (^4) shortL


euler87 = length $ nub $ [ res | 
            a <- l1, b <- l2, c <- l3, let res = a + b + c, res < n]
            
            
            
---- EULER 104 ----- NOT DONE

fib n = table !! n
        where table = 0 : 1 : zipWith (+) table (tail table)

fibs = 1 : scanl (+) 1 fibs

toList x = map digitToInt (show x)

euler104 = [ y | y <- fibs,
             let str = show y,
             let a = take 9 str,
             let b = take 9 (reverse str),
             let func l = and [a `elem` l | a <- ['1'..'9']],
             func a && func b ]

                
                
----- EULER 113 ----- NOT DONE

increasing x = and $ map (\(x,y) -> ( x <= y)) (zip l (tail l))
                  where l = (toList x)
                  
decreasing x = and $ map (\(x,y) -> ( x >= y)) (zip l (tail l))
                  where l = (toList x)
                  

bouncy x = not (increasing x) && not (decreasing x)

euler113 = [ x | x <- [1..10^10], bouncy x]

--- EULER 146 ------


euler146 = [ x | x <- [10,12..], let n = x^2, isPrime (n+1), isPrime (n+3), isPrime (n+7), isPrime (n+9), isPrime (n+13), isPrime (n+27)]

  

