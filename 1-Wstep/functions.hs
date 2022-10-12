import Prelude

-- Zadanie 16
fi n k acc = if k < 1 then acc else if gcd k n == 1 then fi n (k-1) (acc+1) else fi n (k-1) acc
-- version 1
euler n = fi n n 0
totient_sum n acc = if n < 1 then acc else totient_sum (n-1) (acc + (euler n))
-- version 2
euler2 n = length [k | k <- [1..n-1], gcd n k == 1]
totient_sum2 n = sum [euler k | k <- [1..n]]


-- Zadanie 17
allTripletsTo200 = [(a,b,c) | a <- [1..200], b <- [1..a], c <- [1..b]]
triPi = [(a,b,c) | (a,b,c) <- allTripletsTo200, a * a == b * b + c * c, gcd b c == 1]


-- Zadanie 18
fib1 n = if n == 0 then 0 else if n == 1 then 1 else (fib1 (n-1)) + (fib1 (n-2))
fib2 0 = 0
fib2 1 = 1
fib2 x = fib2 (x-1) + fib2 (x-2)


-- Zadanie 19
newton n k = if k == 0 || k == n then 1 else (newton (n-1) (k-1)) + (newton (n-1) k)


-- Zadanie 20
-- version 1
sum_of_divs n d = if d <= 1 then 1 else (sum_of_divs n (d-1)) + (if mod n d == 0 then d else 0)
get_all_perfect n = if n <= 1 then 0 else get_all_perfect (n-1) + (if sum_of_divs n (n-1) == n then 1 else 0)
-- version 2
divisors n = [k | k <- [1..n-1], mod n k == 0]
sumDivisors n = sum $ divisors n    -- $ == precedence, czyli najpierw wykonuje się funkcja po prawej
checkPerfect n = n == sumDivisors n
perfectNumbersTo10K = [n | n <- [1..10000], checkPerfect n]


-- Zadanie 26
-- fib_help n f0 f1 = if n <= 0 then f0 else if n == 1 then f1 else fib_help 
-- fib n = fib_help n 0 1


-- cdn.



