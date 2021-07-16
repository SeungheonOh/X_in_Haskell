module Main where

sayHello :: IO()
sayHello = putStrLn "Hello from Function"

main :: IO ()
main =
  putStrLn "Hello, Haskell!" -- putStrLn simply writes String on the terminal!
  >> sayHello -- and of course we can use function to say hello
  >> do -- Also, instead of using sequencing, do notaion is available for better readablility
    putStrLn "Hello, Haskell! with do notation!"
    sayHello
  >> pure "Hello, Haskell! with binding!" -- let's tastes some binding operator here!
  >>= putStrLn -- this putStrLn gets the value from previous sequence by binding. 