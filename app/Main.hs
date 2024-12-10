module Main where
import Driver (driver, parse_args)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let day = parse_args args
  driver day
