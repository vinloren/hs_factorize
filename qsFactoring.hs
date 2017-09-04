import Factors
import System.Random
import Data.Time
-- import Data.Time.Clock.POSIX
-- import System.IO.Unsafe
-- import System.Time

findPrime p = if (testp p) == [0] then p else findPrime (p+2)

testp :: Integer -> [Integer]
testp p = [testa [x | b <- [2,3,5,7,11,13,17,19,23,31,37,41,53,61], let x = (powm b (p `div` 2) p 1)^2 `mod` p, x > 1]]

-- test if the result from testp was empty list in which case return 0, else head[xs]
testa :: [Integer] -> Integer
testa []     = 0
testa xs = head xs

-- Factorize semi prime with Quadratic Sieving method

main = do
  putStrLn "Decimal digits for the two primes to be generated?"
  inp <- getLine
  let dig = read(inp)
  nu <- randomRIO(10^dig, 10^(dig+1)-1)
  let p = if nu  `mod` 2 == 1 then nu else nu + 1
  let fact1 = findPrime p
  nu <- randomRIO(10^dig, 10^(dig+1)-1)
  let q =  if nu `mod` 2 == 1 then nu else nu + 1
  let fact2 = findPrime q
  let n = fact1*fact2
  putStr "Semi prime: "
  print (show n)
  putStrLn "B primes size?"
  inp <- getLine
  let bsz = read(inp)
  let bp = filtB (fndPrimes [2..bsz] []) n
  print bp
  start <- getCurrentTime
  let r = radix n 2 0
  let scn = scany n r bp 1 (2*r) (length bp) []
  putStrLn "Scanned sieve:"
  print (length(scn))
  print scn
  let p2 = pow2 (toInteger(length(bp))-1) []
-- print p2
  let bf = getBl scn p2 []
  let rs = replE scn bf []
--  putStrLn "r, y, Exp binary matrix:"
--  print rs
--  putStrLn "Sorted scn :"
  let srt3 = sort3 rs (0,0,(-1)) []
--  print srt3
  let sol = rgs srt3 0 (length(scn)-1) (length bp) (length(scn)-1)
--  putStrLn "Solution:"
--  print sol
--  let fin = sols sol 0 0  (length bp)
--  print fin
  putStr ("N. solutions: ")
  print (length sol)
  let rslt = resolv sol n
  putStr ("found (p,q): ")
  print rslt
  end <- getCurrentTime
  putStr "Runnin time (secs.) = "
  print (diffUTCTime end start)

  
  
  
  