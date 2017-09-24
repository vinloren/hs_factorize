-- Factorization by Pollard p-1 algorithm
import System.Random
import Data.Time


findPrime p = if (testp p) == [0] then p else findPrime (p+2)

testp :: Integer -> [Integer]
testp p = [testa [x | b <- [2,3,5,7,11,13,17,19,23,31,37,41,53,61], let x = (powm b (p `div` 2) p 1)^2 `mod` p, x > 1]]

-- test if the result from testp was empty list in which case return 0, else head[xs]
testa :: [Integer] -> Integer
testa []     = 0
testa xs = head xs

-- fndPrimes: find all primes in [2..interval]. Two lists are supplied:
-- the first contains the integer span interval from which the primes
-- will be extracted, the second one is the resulting primes list. The
-- funcion is done when the interval list gets empty.
fndPrimes :: [Integer] -> [Integer] -> [Integer]
fndPrimes [] ol = ol
fndPrimes il ol = fndPrimes [x | x <- tail(il), (x `mod` head(il)) > 0] (ol++head(il):[])


-- compute modular power by using the quadratic sequence based on the least significant bit
-- of the exponent. If 0 accumulate b*b in b, if 1 accumulate b*r in r. The exp. 'e' is 
-- right shifted at each iteraction until it reaches 0 where the resul r is given back. 
powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

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


-- isSqre: test if a Integer is a square.
isSqre :: Integer -> Bool
isSqre i = if (i == r*r) then True else False where r = (radix i 2 0)

radix :: Integer -> Integer -> Integer -> Integer
radix n r1 r2 = if abs(r1-r2) > 1 then (radix n ((r1+(n `div` r1)) `div` 2) (n `div` r1)) else r1

-- END SQFOF


getopt = do 
  putStr "1) gimme semi-prime then run k!\n2) gimme semi-prime then run B primes\n3) gen semi-prime then run k!\n"
  putStrLn "4) gen semi-prime then run B primes\n5) gimme semi-prime then run sqfof\n6) gen semi-prime then run sqfof"  
  s <-getLine
  if s == "1" || s == "2" || s == "5"
    then do 
      putStrLn "Gimme semi-prime"
      inp <- getLine
      let n = read(inp)
      return (n,s)
    else if s /= "3" && s /= "4" && s /= "6" 
      then getopt
      else do
       putStrLn "Decimal digits for the two primes to be generated?"
       inp <- getLine
       let dig = read(inp)   
       let b1 = 10^(dig-1)
       let b2 = 9*10^(dig-1)
       nu <- randomRIO(b1, b2)
       let p = if nu  `mod` 2 == 1 then nu else nu + 1
       let fact1 = findPrime p
       nu <- randomRIO(b1, b2)
       let q =  if nu `mod` 2 == 1 then nu else nu + 1
       let fact2 = findPrime q
       let n = fact1*fact2
       return (n,s)
       
       
main = do
 opt <- getopt
 let n = fst(opt)
 let ty = snd(opt)
 putStr "Semi prime: "
 print (show n)
 start <- getCurrentTime
 if ty == "1" || ty == "3"
   then do 
     let pq = factP n 2 1
     print pq
   else if ty == "1" || ty == "3" 
     then do 
       let primes = fndPrimes [2..floor(8*sqrt (fromIntegral n))] [] 
       let pq = factP' n primes 1
       print pq
     else sqfof n
 end <- getCurrentTime
 putStr "Factors found in "
 print (diffUTCTime end start)