module EulerX where
  
import Data.List
import Data.Char

fact x = product [1..x]

nCr n r = ((fact n) `div` (fact (n-r))) `div` (fact r)
nPr n r = (fact n) `div` (fact (n-r))

fib n = table !! n
        where table = 0 : 1 : zipWith (+) table (tail table)

fibs =  0 : 1 : zipWith (+) fibs (tail fibs)

fibg n = round $ (gratio ^ n) / (sqrt 5)
fibgs = map fibg [1..]
gratio = ((1 + sqrt(5))/2)

tD x    = map digitToInt (show x)
tL xs   = map intToDigit xs 


umap :: [Int] -> Integer -> Integer
unmap xs = umap xs (10^((length xs)-1))
umap xs n
            | xs == [] = 0
            | otherwise = (fromIntegral (head xs) * n) + (umap (tail xs) n `div` 10)
            
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors n = factor n primes
              where
                factor n (p:ps) 
                    | p*p > n        = [n]
                    | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
                    | otherwise      = factor n ps


isPrime x = if x <= 1 then False else length(primeFactors x) == 1
isPrime2 x = elem x (takeWhile (<=x) primes)

properDivisors = init . nub . (map product) . subsequences . primeFactors


isPandigital x n =  isPan (tD x) n 1

isPanL x n = isPan x n 1

isPan xs n a
                | a > n = True
                | a `elem` xs = isPan xs n (a+1) 
                | otherwise = False
                
                
                
                


--- EULER 15 --- DONE
euler15 = nCr 40 20

problem_15 = iterate (scanl1 (+)) (repeat 1) !! 20 !! 20


--- EULER 24 --- DONE

euler24 = (sort $ permutations [0,1,2,3,4,5,6,7,8,9]) !! 999999



--- EULER 31 --- NOT DONE


coins x [] = 0
coins amount (x:xs)     | amount == 0 = 1
                        | amount < 0 = 0
                        | otherwise = (coins amount xs) + (coins (amount-x) (x:xs))


--- EULER 37 --- DONE

truncLeft 0     = True
truncLeft x     = (isPrime x) && (truncLeft $ (unmap $ tail $ tD x))

truncRight 0    = True
truncRight x    = (isPrime x) && (truncRight $ (unmap $ init $ tD x))

euler37 = [x | x <- primes, truncLeft x, truncRight x]



--- EULER 39 --- DONE

solve39 p = [   (a,b,c) | a <- [1..p], b <- [a..p], c <- [b..p],
                a^2 + b^2 == c^2, a+b+c == p]     

solve39_ z p= [ [a,b,c] |    m <- [1..z], n <- [1..(m-1)],
                            k <- [1..20],
                            let a = k* (m^2 - n^2),
                            let b = k*2*m*n,
                            let c = k*(m^2 + n^2),
                            a+b+c == p]

poss39 x = length $ nub $ map sort $ solve39_ 25 x

e39 = sort $ map (\x -> (poss39 x,x)) [1..1000]

--- EULER 23 ---

abundant x = (sum $ properDivisors x) > x
abundants = [x | x<- [1..28123], abundant x]



--- EULER 43 --- DONE

l43 = [0..9]
e43 = [ res |   d1 <- l43, 
                d2 <- l43 \\ [d1],
                d3 <- l43 \\ [d1,d2],
                d4 <- [0,2,4,6,8] \\ [d1,d2,d3],
                d5 <- l43 \\ [d1,d2,d3,d4],
                d6 <- [0,5] \\ [d1,d2,d3,d4,d5],
                d7 <- l43 \\ [d1,d2,d3,d4,d5,d6],
                d8 <- l43 \\ [d1,d2,d3,d4,d5,d6,d7],
                d9 <- l43 \\ [d1,d2,d3,d4,d5,d6,d7,d8],
                d10 <- l43 \\ [d1,d2,d3,d4,d4,d6,d7,d8,d9],
                let res = [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10],
                (d3 + d4 + d5) `mod` 3 == 0,
                (10*d5 + d6 - 2*d7) `mod` 7 == 0,
                (10*d7+ d8 + d6) `mod` 11 == 0,
                (10*d7 + d8 + 4*d9) `mod` 13 == 0,
                (10*d8 + d9 - 5*d10) `mod` 17 == 0
                ]
                
euler43 = sum $ map unmap e43



--- EULER 49 --- DONE - SLOW!!

primes49 = dropWhile (<1000) $ takeWhile (<10000) primes

euler49 = [(x,y,z) | 
                x <- primes49, 
                y <- dropWhile (<=x)     primes49, 
                z <- dropWhile (<=y) primes49,
                z - y == y - x,
                (tD y) `elem` (permutations $ tD x),
                (tD z) `elem` (permutations $ tD y)]
                
                
--- EULER 58 --- DONE

diagonals :: [[Integer]]
diagonals = map (\n -> [n^2 - 3*n + 3, n^2 - 2*n + 2, n^2 - n + 1, n^2]) [3,5..]
count xs = length $ filter (==True) $ map isPrime xs

eu58 = e58 diagonals 1 0
e58 (x:xs) total numprimes = (x, fromIntegral p / t) : e58 xs t p
                                     where  p = numprimes + count x
                                            t = total + 4

euler58 = head [sqrt $ fromIntegral $ last $ xs | (xs,percent) <- eu58, percent < 0.1]


--- EULER 63 --- DONE

euler63 = length [ k |      x <- [1..9], 
                            y <- [1..22],
                            let k = x^y,
                            length (tD k) == y ]
                            


--- EULER 92 --- NOT DONE

-- os is the 1's buffer, es is the 89 buffer
-- buf is the current execution buffer
-- n is the current number being evaluated
-- x is the starting number, chain loops for 1 -> 10^7

e92 = chain 1 1 [] [1] [89]

chain x n buf os es     | x == bound = (nub os, nub es)
                        | n == 1  || n `elem` os    = chain next next [] (os ++ n:buf) es
                        | n == 89 || n `elem` es    = chain next next [] os (es ++ n:buf)
                        | otherwise = chain x k (x:buf) os es
                            where   next = x + 1
                                    bound = 10^4
                                    k = sum $ map (^2) (tD n)
                                    
                                    


--- EULER 94 --- NOT DONE   
heron a b c = sqrt $ (s*(s-a)*(s-b)*(s-c))
                        where s = (a+b+c)/2
                        
e94 = [ ((x,a,y),(x,b,z)) |     x <- [1..333333333],
                                let a = x + 1,
                                let b = x - 1,
                                let y = heron x x a,
                                let z = heron x x b]
                                

--- EULER 104 ---
euler104 = [ y |  y <- [2749..], let x = fibg y,
                isPandigital (x `mod` 10^9) 9,
                isPanL (take 9 (tD x)) 9]
 
 
--- EULER 124 ---

radical n = product $ nub $ primeFactors n
e124 = [ (radical x , x) | x <- [1..100000]]
euler124 = (sort e124) !! 9999


--- EULER 127 ---


--- EULER 145 ---

euler145 = [ x |    x <- [1..10^9],
                    x `mod` 10 /= 0,
                    let y = tD $ x + unmap (reverse $ tD x),
                    and $ map odd $ y]

p145 n = all odd $ map digitToInt $ show (n + read (reverse $ show n))
e145 = [ x | x <- [1..10^9], p145 x]

--- EULER 187 --- NOT DONE

euler187 = [ x | x <- [1..10^8], length (primeFactors x) == 2]        

--- EULER 203 --- DONE


-- doesn't work, dunno
pascals = [[1]] ++ [[1,1]] ++ [[1,2,1]] ++ [ 1 : (zipWith (+) (last pascals) (tail $ last pascals)) ++ [1] ]


-- gets the first m rows of pascals triangle
pascal m = pas [[1],[1,1]] 2 m
pas buf n m     | n == m = buf
                | otherwise = pas (buf ++ [next]) (n+1) m
                        where next = 1 : zipWith (+) (last buf) (tail $ last buf) ++ [1]

-- note: we use takeWhile (<= 51) as the highest possible prime factor will be < 51
squarefree n = null [ n | k <- takeWhile (<= 51) $ map (^2) primes, n `mod` k == 0]

concatPascal m = concat $ pascal m

euler203 = sum [ k | k <- nub $ concatPascal 51, squarefree k]
    

--- EULER 205 --- DONE

p205 = [1..4]
c205 = [1..6]

peter :: Int -> Int
peter n = length $  [ (a,b,c,d,e,f,g,h,i) |   a <- p205,
                                    b <- p205,
                                    c <- p205,
                                    d <- p205,
                                    e <- p205,
                                    f <- p205,
                                    g <- p205,
                                    h <- p205,
                                    i <- p205,
                                    a+b+c+d+e+f+g+h+i == n]
                                    
                                    
colin :: Int -> Int                                    
colin n = length $ [ (a,b,c,d,e,f) |     a <- c205,
                                b <- c205,
                                c <- c205,
                                d <- c205,
                                e <- c205,
                                f <- c205,
                                a+b+c+d+e+f == n]
                                

-- Used the above to precalculate these....

pete n = [0,0,0,0,0,0,0,0,0,1,9,45,165,486,1206,2598,4950,8451,13051,18351,23607,27876,30276,30276,27876,23607,18351,13051,8451,4950,2598,1206,486,165,45,9,1] !! n
coli n = [0,0,0,0,0,0,1,6,21,56,126,252,456,756,1161,1666,2247,2856,3431,3906,4221,4332,4221,3906,3431,2856,2247,1666,1161,756,456,252,126,56,21,6,1] !! n

pp = (1/4)^9  -- probability peter
pc = (1/6)^6  -- probability colin

e205 = sum [ res  |
                        a <- [36,35..9],
                        b <- [a-1,a-2..6],
                        let res = pp * fromIntegral (pete a) * pc * fromIntegral (coli b)]
                        


--- EULER 206 --- DONE - BRUTE FORCE

euler206 = head [ x | 
                x <- [1000000000,1000000010..],
                let k = tD (x ^ 2),
                k !! 0 == 1,
                k !! 2 == 2,
                k !! 4 == 3,
                k !! 6 == 4,
                k !! 8 == 5,
                k !! 10 == 6,
                k !! 12 == 7,
                k !! 14 == 8,
                k !! 16 == 9]
           
--- EULER 254 --- NOT DONE

f n     = sum $ map fact $ tD n
sf n    = sum $ tD $ f n
g n     = head [ x | x <- [1..], sf x == n]
sg n    = sum $ tD $ g n


--- EULER 268 --- NOT DONE
--e268 = length $ [ a | a <- [1..10^16 - 1], (length $ takeWhile (<100) $ nub $ primeFactors a) >= 4]

p268 = takeWhile (<100) primes
e268 n = length (take 4 [ x | x <- p268, n `mod` x == 0]) == 4
euler268 = length [ x | x<-[1..10^16 - 1], e268 x]


                                    



