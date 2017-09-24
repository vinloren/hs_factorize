## Semi-prime factorization by Quadratic Sieving
This project is aimed to test haskell feasibility in developing a factorization program aimed to 
discover the p q prime factors of a semi-prime integers as big as 10 - 20 decimal digits or more.

The available literature regarding this approach tells it is viable for semi-primes from 20 up to 40 
decimal digits so I was interested to implement it using haskell as I did in the past using C and 
mpir libraries on Linux.

Since haskell is capable to manage big integers of any size, I was enticed to try it targeting the QS factorization.

### The QS approach
Factorization of big semi-primes is very difficult since there are'nt maths algorithms available to 
get a positive result in reasonable time if the size of the semi-prime is beyond 140 dec. digits even 
using a nerwork of fast computers. This is why the RSA cipher 2048 bits is considered practically "unbreakable" 
also nowadays.

The QS sieving approach was one of the historical attempts made in search of a way to factorize big integers and 
showed to be applicable to semi-primes up to 80 decimal digits distributing the task of "sieving"  among a 
network of computers each working on a fraction of the entire sieving span.

### Basic concept
The basic concept is simple, yet clever, and recalls the product of two binomies:<br>
<center><b>(a+b)*(a-b) = a^2-b^2.</b></center><br>
So the assuption is that there must exist a square b^2 = a^2 - n where n is the semi-prime to be factorize 
, a = ceiling(sqrt n). Once b is found then the two factors of n will be p=(a-b), q=(a+b). Example: let's take n = 8051 
, ceiling sqrt(n) = a = 90 (90^2 = 8100), b^2 = 8100 - 8051 = 49 (a perfect square) then b = 7 and 
p = (90-7) = 83, q = (90+7 = 97 (83*97 = 8100).

The example just shown belongs to the lucky case where a square is found rightaway, in practice it would take 
a much longer iteration with (a+1)^2, (a+2)^2, (a+x)^2 as we could see in another example:<br>
Let's take 27523: r=166, 166^2=27556, r^2-n = 33, 33 is not asquare so we need to go on:<br>
r       r^2-n   factors
166     33      3x11
167     366     2x3x61
168     701     701
169     1038    





let's take n = 1100017, ceiling(sqrt 1100017) = 1049, n-1049^2 = 384 which is not a square.
We could then go on testing 1050,1051,1052... hoping to find a square in r^2-n but doing so 
it caould take long. There : <br>
r       n-r^2
1049    384



