import Factors
import System.Random
import Data.Time
-- import Data.Time.Clock.POSIX
-- import System.Time

findPrime p = if (testp p) == [0] then p else findPrime (p+2)

testp :: Integer -> [Integer]
testp p = [testa [x | b <- [2,3,5,7,11,13,17,19,23,31,37,41,53,61], let x = (powm b (p `div` 2) p 1)^2 `mod` p, x > 1]]

-- test if the result from testp was empty list in which case return 0, else head[xs]
testa :: [Integer] -> Integer
testa []     = 0
testa xs = head xs

ceil :: Float -> Integer
ceil x = ceiling(x)

-- Factorize semi prime with Quadratic Sieving method

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
 let b = log (fromIntegral n)
 let f = 0.45
 let sqb = f * sqrt (b)
 let lgb = sqrt (log (b))
-- print lgb
 let bsz = round (exp (sqb * lgb))
 let lgbsz = log (exp (sqb * lgb))
 let nf = round((fromIntegral bsz) / lgbsz)
 putStr "Max B : "
 print bsz
 putStr "Num factors:"
 print nf
 let bp = take nf (filtB (fndPrimes [2..2*bsz] []) n)
 print bp
 start <- getCurrentTime
 let r = radix n 2 0
 let scn = scany n r bp 1 (2*r) (length bp) []
 putStrLn "Scanned sieve:"
 print (length(scn))
-- print scn
 siev <- getCurrentTime
 putStr "Scan done in "
 print (diffUTCTime siev start)
 let ln = toInteger(length bp)
 let p2 = pow2 (ln-1) []
 let bf = getBl scn p2 []
 let rs = replE scn bf []
 let srt3 = sort3 rs (0,[0],(-1)) []
 let sol = rgs n srt3 0 (length(scn)-1) (length bp) (length(scn)-1)
 putStr ("N. solutions: ")
 print (length sol)
 red <- getCurrentTime
 putStr "Matrix reduced in "
 print (diffUTCTime red siev)
-- print sol
 let rslt = (resolv sol n bp)
 putStr "found (p,q): "
 print rslt
 end <- getCurrentTime
 putStr "Factors found in "
 print (diffUTCTime end start)

       

