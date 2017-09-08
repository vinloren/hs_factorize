-- factorizing support functions
module Factors
( radix
, fndPrimes
, isSqre
, filtB
, tsmth
, scany
, pow2
, powm
, getBl
, replE
, sort3
, rgs
, resolv
) where

import Data.Bits
-- import Debug.Trace

-- radix: find integer square root of integers by Newton method
-- the final result is found when r1 becomes == r2 after a few
-- iterations. Initially r1 = 2 r2 = 0 whatever 'n' be. The radix
-- is the arithmetic mean between r1 and r2 quickly converging to
-- r1 = r2 or very close to, when this is achieved then the funcion is done.
radix :: Integer -> Integer -> Integer -> Integer
radix n r1 r2 = if abs(r1-r2) > 1 then (radix n ((r1+(n `div` r1)) `div` 2) (n `div` r1)) else r1

-- fndPrimes: find all primes in [2..interval]. Two lists are supplied:
-- the first contains the integer span interval from which the primes
-- will be extracted, the second one is the resulting primes list. The
-- funcion is done when the interval list gets empty.
fndPrimes :: [Integer] -> [Integer] -> [Integer]
fndPrimes [] ol = ol
fndPrimes il ol = fndPrimes [x | x <- tail(il), (x `mod` head(il)) > 0] (ol++head(il):[])

-- get list of Integers pairs where fst = x, snd = x^2
prSqre :: [Integer] -> [(Integer,Integer)]
prSqre il = [(x,x*x) | x <- il]

-- isSqre: test if a Integer is a square.
isSqre :: Integer -> Bool
isSqre i = if (i == r*r) then True else False where r = (radix i 2 0)

-- filtB: filter primes in list B purging those not supplying a quadratic residue
-- to n mod p
filtB :: [Integer] -> Integer -> [Integer]
filtB bl n = [x | x <- bl, 1 == (powm n ((x-1) `div` 2) x 1)] 

-- test smoothness y = (r^2 - n) thru B primes in list 'bl'
tsmth :: Integer -> Integer -> [Integer] -> [Integer] -> [Integer]
tsmth 1 e [] rl = rl
tsmth _ 0 [] _ = []
tsmth n e bl rl = if n `mod` head(bl) == 0 
  then (tsmth (n `div` head(bl)) (e+1) bl rl) 
  else (tsmth n 0 (tail(bl)) (rl++e:[]))

-- scan sequential (radix n + 1,2,3,x)^2 - n to be smooth with B factors
-- getting a list of triples r+x, y (list of factors' exp), s = y but exps
-- are mod 2 for eventual matrix reduction, n is the semi-prime from which
-- y = rn^2 - n is got from, bl is the B primes list, s is the smooth y
-- when found, d is the incremet to radix, top is the limit scan, 
scany :: Integer -> Integer -> [Integer] -> Integer -> Integer -> Int -> [(Integer,[Integer],[Integer])] -> [(Integer,[Integer],[Integer])]
scany n r bl d top cnt rs = 
  if d > top || cnt < 0
    then rs 
    else if s /= [] 
      then scany n r bl (d+1) top (cnt-1) rs++(r+d,s,(map (`mod` 2) s)):[]
      else scany n r bl (d+1) top cnt rs
    where 
      y=(r+d)^2 - n
      s = tsmth y 0 bl []
        
-- compute modular power by using the quadratic sequence based on the least significant bit
-- of the exponent. If 0 accumulate b*b in b, if 1 accumulate b*r in r. The exp. 'e' is 
-- right shifted at each iteraction until it reaches 0 where the resul r is given back. 
powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r


-- create list power of 2 up to limit
pow2 :: Integer -> [Integer] -> [Integer]
pow2 top r = if top > -1 
  then pow2 (top-1) (r++(2^top):[])
  else r
      
-- multiply element list a * element list b 
multL :: [Integer] -> [Integer] -> [Integer] -> Integer
multL [] [] r = fromInteger(sum r)
multL a b r = multL (tail(a)) (tail(b)) (((head(b)*(head(a)))):r)

-- get third element in triplets
trd :: (Integer,[Integer],[Integer]) -> [Integer]
trd (_,_,x) = x

-- get third element in triplets
trdi :: (Integer,[Integer],Integer) -> Integer
trdi (_,_,x) = x

-- get second element in triplets
sec :: (Integer,[Integer],[Integer]) -> [Integer]
sec (_,x,_) = x

-- get second element in triplets
seci :: (Integer,[Integer],Integer) -> [Integer]
seci (_,x,_) = x

-- get first element in triplets
fir :: (Integer,[Integer],[Integer]) -> Integer
fir (x,_,_) = x

-- get first element in triplets
firi :: (Integer,[Integer],Integer) -> Integer
firi (x,_,_) = x

-- get S exp factors from triplets from scany
getSf :: (Integer,[Integer],[Integer]) -> [Integer]
getSf r = trd(r)

-- get binary list of S exponent converted to binary integer
getBl :: [(Integer,[Integer],[Integer])] -> [Integer] -> [Integer] -> [Integer]
getBl [] _ r = r
getBl l p2 r = getBl (tail(l)) p2 (r++s:[]) where s = multL p2 (getSf(head(l))) [] 

-- replace exp list in third param of triplets list with corresponding 
-- integer value in getBl
replE :: [(Integer,[Integer],[Integer])] -> [Integer] -> [(Integer,[Integer],Integer)] -> [(Integer,[Integer],Integer)]
replE l [] r = r
replE l b r = replE (tail(l)) (tail(b)) ((fir(head(l)),sec(head(l)),head(b)):r) 

-- fetch triplet with max value in third param
max3 :: [(Integer,[Integer],Integer)] -> (Integer,[Integer],Integer) -> (Integer,[Integer],Integer) 
max3 [] t = t
max3 l t = max3 (tail(l)) (if trdi(head(l)) > trdi(t) then (head(l)) else t)

-- sort3 sort triplets in replE list
sort3 :: [(Integer,[Integer],Integer)] -> (Integer,[Integer],Integer) -> [(Integer,[Integer],Integer)] -> [(Integer,[Integer],Integer)]
sort3 [] _ r = r
sort3 l mi r = sort3 t (0,[0],-1) (r++(m:[])) 
  where 
    m = max3 l mi
    t = [x | x <-l, x /= m]
    
-- redux triplet j to rp in matrix where j is replaced by rp. return sorted matrix
redux :: [(Integer,[Integer],Integer)] -> (Integer,[Integer],Integer) -> (Integer,[Integer],Integer) -> [(Integer,[Integer],Integer)]
redux l j rp = sort3 ([ x | x <- l, x /= j]++rp:[]) (0,[0],(-1)) []

-- sumexp: sum exp list of y factors element to element
sumexp :: [Integer] -> [Integer] -> [Integer] -> [Integer]
sumexp [] [] c = c
sumexp a b c = sumexp (tail(a)) (tail(b)) (c++(head(a) + head(b)):[])

-- fexp get sqrt of y by B factors exps/2 in matrix 
fexp :: [Integer] -> [Integer] -> Integer -> Integer
fexp [] [] r = r
fexp a b r = fexp (tail(a)) (tail(b)) (r*((head(a))^(head(b) `div` 2)))

-- rgs reduce matrix with gauss algoritm. Start from bottom to top in search of first '1'
rgs :: [(Integer,[Integer],Integer)] -> Int -> Int -> Int -> Int -> [(Integer,[Integer],Integer)] 
rgs l i j c r = do
  if i == c
    then sols l 0 0 c
    else do 
      let tg = trdi(l!!j)
      let x = (tg .&. (2^(c-i-1)))
      if x == 0
        then if j > 0
          then rgs l i (j-1) c r
          else rgs l (i+1) r c r
        else if j > i
        then do 
          let t = l!!(j-1)
          let t1 = l!!j
          let p = trdi(t1) 
          let q = trdi(t)
          let b = (firi(t)*firi(t1),sumexp (seci(t1)) (seci(t)) [],(xor p q))  -- modificare seci(t1)*seci(t) in lista somma esponenti di fatt. y
          let rdx = redux l t b
          rgs rdx i j c r
        else rgs l (i+1) r c r


-- sols: find solutions in the matrix received by rgs
sols :: [(Integer,[Integer],Integer)] -> Int -> Int -> Int -> [(Integer,[Integer],Integer)]
sols l i j c = do
  if i == c
    then [x | x <- l, trdi(x) == 0]
    else do
      let cl = 2^(c-i-1)
      let tg = (trdi(l!!j))
      if (cl .&. tg) > 0 && i == j
        then sols l (i+1) (j+1) c
        else if (cl .&. tg) == 0 && i == j
          then sols l (i+1) j c
          else if (cl .&. tg) == 0 && i /= j
            then sols l (i+1) j c
            else if i < c 
              then do
                let tg1 = (trdi(l!!(j+1)))
                if (cl .&. tg1) > 0
                  then do
                    let a = (firi(l!!j),seci(l!!j),tg)
                    let b = (firi(l!!j)*firi(l!!(j+1)),sumexp (seci(l!!j)) (seci(l!!(j+1))) [],(xor tg tg1))
                    let rdx = redux l a b
                    sols rdx i j c
                else sols l (i+1) (j+1) c
            else sols l (i+1) (j+1) c
   
-- find the factors p,q of the target semi prime. l is list of (r, r^2-y, exps) from 
-- resolved matrix
resolv :: [(Integer,[Integer],Integer)] -> Integer -> [Integer] -> [(Integer,Integer)]
resolv l m bl = [rsv x m | x <- l, let rsv x m = (p,q) where p = (gcd (firi(x) - r) m); r = fexp bl (seci(x)) 1; q = m `div` p]

-- glenr: get number of dec digits from r in list of solution
glenr :: [(Integer,[Integer],Integer)] -> [Integer] -> [Integer]
glenr [] r = r
glenr l r = glenr (tail l) (lg:r) where lg = ceiling(logBase 10 (fromIntegral (firi(head(l)))))


 