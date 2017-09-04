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
  
  
  
  
  