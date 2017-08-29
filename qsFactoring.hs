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
  print (length(scn))
  print scn
  
  
  