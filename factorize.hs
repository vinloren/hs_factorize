-- Factorization by Pollard p-1 algorithm, squfof, quadratic sieve
import Factors
import System.Random
import Data.Time


findPrime p = if (testp p) == [0] then p else findPrime (p+2)

testp :: Integer -> [Integer]
testp p = [testa [x | b <- [2,3,5,7,11,13,17,19,23,31,37,41,53,61], let x = (powm b (p `div` 2) p 1)^2 `mod` p, x > 1]]

-- test if the result from testp was empty list in which case return 0, else head[xs]
testa :: [Integer] -> Integer
testa []     = 0
testa xs = head xs


getopt = do 
  putStr " 1) gimme semi-prime then run k!\n 2) gimme semi-prime then run B primes\n 3) gen semi-prime then run k!\n"
  putStr " 4) gen semi-prime then run B primes\n 5) gimme semi-prime then run sqfof\n 6) gen semi-prime then run sqfof\n"  
  putStrLn " 7) gimme semi-prime then run qs\n 8) gen semi-prime then run qs\n 9) gimme semi-prime then run ecm\n10) gen semi-prime then run ecm"
  s <-getLine
  if s == "1" || s == "2" || s == "5" || s == "7" || s == "9"
    then do 
      putStrLn "Gimme semi-prime"
      inp <- getLine
      let n = read(inp)
      return (n,s)
    else if s /= "3" && s /= "4" && s /= "6" && s /= "8" && s /= "10"
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
   then do                -- Pollard p-1 method k!
     let pq = factP n 2 1
     print pq
   else if ty == "2" || ty == "4" 
     then do              -- Pollard p-1 method LCM
       let primes = fndPrimes [2..floor(8*sqrt (fromIntegral n))] [] 
       let pq = factP' n primes 1
       print pq
     else if ty == "5" || ty == "6" 
       then sqfof n        -- Square Form method
       else if ty == "9" || ty == "10" 
        then do            -- Lenstra Elliptic Curve Method
          let k = lcmB n
--        putStr "got k in "
--        gotk <- getCurrentTime
--        print (diffUTCTime gotk start)
          let rs = ecm n k [1..1501]
          print rs
        else do           -- Quadratic Sieving
         let b = log (fromIntegral n)
         let f = if n < 10^9 then 0.63 else if n < 10^13 then 0.58 else if n < 10^16 then 0.56 else if n < 10^19 then 0.52 else if n < 10^22 then 0.51 else 0.49
         let sqb = f * sqrt (b)
         let lgb = sqrt (log (b))
         let bsz = round (exp (sqb * lgb))
         putStr "Max B: "
         print bsz
         let bp = filtB (fndPrimes [2..bsz] []) n
         putStrLn ("B factors len: "++(show (length bp)))
--       print bp
         let r = radix n 2 0
         let scn = scany n r bp 1 (length bp) []
         putStrLn "Scanned sieve:"
         print (length(scn))
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
         let rslt = (resolv sol n bp)
         putStr "found (p,q): "
         print rslt 
 end <- getCurrentTime
 putStr "Factors found in "
 print (diffUTCTime end start)