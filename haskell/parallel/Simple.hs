import Control.Parallel.Strategies

primes :: [Int]
primes = [x | x <- [2..], let root = floor $ sqrt $ fromIntegral x, all ((/=0) . mod x) [2..root]]

testPrimes :: [Int]
testPrimes = take (10^5) primes

parPrimes = testPrimes `using` parListChunk 5000 rdeepseq

main = do
  putStrLn "Start:"
  print (sum testPrimes)
