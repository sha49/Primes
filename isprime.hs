import Primes
import System.Environment
import System.Random

main = do x <- getArgs
          seed <- newStdGen
          print $ isPrime seed $ rInteger $ head x

rInteger :: String -> Integer
rInteger = read 