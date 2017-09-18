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

getopt = do 
  putStrLn "1) gimme semi-prime\n2) generate semi-prime"
  s <-getLine
  if s == "1" 
    then do 
      putStrLn "Gimme semi-prime"
      inp <- getLine
      let n = read(inp)
      return n
    else if s /= "2"
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
       return n
       
       
main = do
 n <- getopt
 putStr "Semi prime: "
 print (show n)
 start <- getCurrentTime
--let pq = factP n 2 1
 let primes = fndPrimes [2..floor(8*sqrt (fromIntegral n))] []  
-- print primes
 let pq = factP' n primes 1
 print pq
 end <- getCurrentTime
 putStr "Factors found in "
 print (diffUTCTime end start)