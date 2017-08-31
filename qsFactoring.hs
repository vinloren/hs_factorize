import Factors

-- Factorize semi prime with Quadratic Sieving method

main = do
  putStrLn "Gimme semi-prime"
  inp <- getLine
  let n = read(inp)
  putStrLn "B primes size?"
  inp <- getLine
  let bsz = read(inp)
  let bp = filtB (fndPrimes [2..bsz] []) n
  print bp
  let r = radix n 2 0
  let scn = scany n r bp 1 (2*r) (length bp) []
  putStrLn "Scanned sieve:"
  print (length(scn))
  print scn
  let p2 = pow2 (length(bp)-1) []
-- print p2
  let bf = getBl scn p2 []
  putStrLn "Exp binary matrix:"
  print bf
  
  
  