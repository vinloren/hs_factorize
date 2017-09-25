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
, factP
, factP'
, sqfof
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
scany :: Integer -> Integer -> [Integer] -> Integer -> Int -> [(Integer,[Integer],[Integer])] -> [(Integer,[Integer],[Integer])]
scany n r bl d cnt rs = 
  if y > 2*n || cnt < 0
    then rs 
    else if s /= [] 
      then scany n r bl (d+1) (cnt-1) rs++(r+d,s,(map (`mod` 2) s)):[]
      else scany n r bl (d+1) cnt rs
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
fexp :: Integer -> [Integer] -> [Integer] -> Integer -> Integer
fexp m [] [] r = r
fexp m a b r = fexp m (tail(a)) (tail(b)) ((r*((head(a))^(head(b) `div` 2))) `mod` m)


-- rgs reduce matrix with gauss algoritm. Start from bottom to top in search of first '1'
rgs :: Integer -> [(Integer,[Integer],Integer)] -> Int -> Int -> Int -> Int -> [(Integer,[Integer],Integer)] 
rgs n l i j c r = do
  if i == c
    then sols n l 0 0 c
    else do 
      let tg = trdi(l!!j)
      let x = (tg .&. (2^(c-i-1)))
      if x == 0
        then if j > 0
          then rgs n l i (j-1) c r
          else rgs n l (i+1) r c r
        else if j > i
        then do 
          let t = l!!(j-1)
          let t1 = l!!j
          let p = trdi(t1) 
          let q = trdi(t)
          let b = (((firi(t)*firi(t1)) `mod` n),sumexp (seci(t)) (seci(t1)) [],(xor p q))  
          let rdx = redux l t b
          rgs n rdx i j c r
        else rgs n l (i+1) r c r

multm :: Integer -> Integer -> Integer -> Integer
multm a b n = (a*b) `mod` n

-- sols: find solutions in the matrix received by rgs
sols :: Integer -> [(Integer,[Integer],Integer)] -> Int -> Int -> Int -> [(Integer,[Integer],Integer)]
sols n l i j c = do
  if i == c
    then [x | x <- l, trdi(x) == 0]
    else do
      let cl = 2^(c-i-1)
      let tg = (trdi(l!!j))
      if (cl .&. tg) > 0 && i == j
        then sols n l (i+1) (j+1) c
        else if (cl .&. tg) == 0 && i == j
          then sols n l (i+1) j c
          else if (cl .&. tg) == 0 && i /= j
            then sols n l (i+1) j c
            else if i < c 
              then do
                let tg1 = (trdi(l!!(j+1)))
                if (cl .&. tg1) > 0
                  then do
                    let a = (firi(l!!j),seci(l!!j),tg)
                    let b = ((firi(l!!j)*firi(l!!(j+1)) `mod` n),sumexp (seci(l!!j)) (seci(l!!(j+1))) [], (xor tg tg1))
                    let rdx = redux l a b
                    sols n rdx i j c
                else sols n l (i+1) (j+1) c
            else sols n l (i+1) (j+1) c
   
-- find the factors p,q of the target semi prime. l is list of (r, r^2-y, exps) from 
-- resolved matrix
resolv :: [(Integer,[Integer],Integer)] -> Integer -> [Integer] -> [(Integer,Integer)]
resolv l m bl = [rsv x m | x <- l, let rsv x m = (p,q) where p = gcd (abs(firi(x) - r)) m; r = fexp m bl (seci(x)) 1; q = m `div` p]


-- SQFOF algoritm
sqfof n = do
  let k = [1,2,3,5,7,11,13,17,19] -- ,3*5,3*7,3*11,5*7,5*11,7*11,3*5*11,3*7*11,3*5*7*11
      s = sqrt (fromIntegral n)
      l = 3*floor(sqrt 2*s)
      p = radix ((head k)*n) 2 0
      q = ((head k)*n)-p^2
      i = 2
      res = sqfr n k l p (p,p) (1,q) i
  print res
      
      
sqfr :: Integer -> [Integer] -> Integer -> Integer -> (Integer,Integer) -> (Integer,Integer) -> Integer -> (Integer,Integer,Integer)
sqfr n [] l p0 p q i = (0,0,i)
sqfr n k l p0 p q i = if (i < l)
  then do 
    let b = (p0+snd(p)) `div` snd(q)
        p1 = b*snd(q) - snd(p)
        qp = snd(q)
        q1 = fst(q) + b*(snd(p) - p1)
    if (mod i 2) == 1
      then sqfr n k l p0 (snd(p),p1) (qp,q1) (i+1)
      else if (isSqre q1)
       then do
        let r = radix q1 2 0
            qp2 = r  -- if (mod r 2) == 0 then r `div` 2 else
            b1 = (p0 - p1) `div` qp2
            p2 = b1*qp2 + p1
            pp1 = p2
            q2 = ((head k)*n - pp1^2) `div` qp2
        sqfofr k n p0 p2 pp1 qp2 q2
       else sqfr n k l p0 (snd(p),p1) (qp,q1) (i+1) 
  else sqfr n (tail k) l p01 (p01,p01) (1,(kn-(p01^2))) 2 
  where
    p01 = radix ((head k)*n) 2 0
    kn = (head k)*n
    
    
sqfofr :: [Integer] -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer,Integer,Integer)
sqfofr k n p0 p pp qp q = do
  let b = (p0+p) `div` q
      pp1 = p
      p1 = b*q - p
      qp1 = q
      q1 = qp + b*(pp1 - p1)
  if (p1 /= pp1)
    then sqfofr k n p0 p1 pp1 qp1 q1
    else do 
      let g = gcd n qp1
      if g == 1
        then do
          let k1 = tail k
          if k1 == []
            then (0,0,0)
            else do
              let pn = radix ((head k1)*n) 2 0
              let qn = ((head k1)*n)-pn^2
              sqfr n k1 (3*floor(sqrt 2*sqrt (fromIntegral n))) pn (pn,pn) (1,qn) 2
        else (g,n `div` (g),head k)
-- END SQFOF

-- Pollard p-1 factorization function. k starts from 2 incremented by 1 each step, f = k!
factP :: Integer -> Integer -> Integer -> (Integer,Integer,Integer)
factP n k f = if (g == n)
  then (0,0,k) 
  else if g == 1
    then factP n (k+1) ((k*f) `mod` n)
    else (g,n `div` g,k)
  where 
    r = (2^k `mod` n)*(2^f `mod` n)
    g = gcd (r-1) n
    
factP' :: Integer -> [Integer] -> Integer -> (Integer,Integer,Integer)
factP' n [] s = (0,0,s)
factP' n k s = if g == 1 || g == n
  then factP' n (tail k) ((s*m) `mod` n)
  else (g,n `div` g,h)
  where
   h = head k
   lgk = floor(logBase(fromIntegral h) (fromIntegral(n)))
   m = s*lgk*h
   r = (2^s `mod` n)*(2^lgk `mod` n)*(2^h `mod` n) - 1
   g = gcd r n


 