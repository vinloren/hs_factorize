-- factorizing support functions

module Factors
( radix
, fndPrimes
, isSqre
, filtB
, tsmth
, scany
) where

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

-- isSqre: test if a Integer is a square. This function is useful to detect
-- prime factors suitable to be enlisted in B smooth primes used to collect
-- r^2 - n equations to populate the sieve (as big as the # of primes in B +1)
-- The prime in B is suitable if (n mod p) = a square otherwise it is skipped
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
-- getting a list of triples r+x, y, s factors list. n is the semi-prime
-- to get y = rn^2 - n from, bl is the B primes list, s is the smooth y
-- when found, d is the incremet to radix, top is the limit scan, 
scany :: Integer -> Integer -> [Integer] -> Integer -> Integer -> Int -> [(Integer,Integer,[Integer])] -> [(Integer,Integer,[Integer])]
scany n r bl d top cnt rs = 
  if d > top || cnt < 0
    then rs 
    else if s /= [] 
      then scany n r bl (d+1) top (cnt-1) rs++(r+d,y,(map (`mod` 2) s)):[]
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
      
      

