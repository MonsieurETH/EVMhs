module Main (main) where

import Lib

main :: IO ()
main = do
  putStrLn "Enter bytecode:"
  input <- getLine
  let initialVm = Lib.newEVM
      maybeUpdatedVm = Lib.run initialVm Nothing Nothing input
  case maybeUpdatedVm of
    Just updatedVm -> putStrLn $ "Updated VM: " ++ show updatedVm
    Nothing -> putStrLn "Error: Execution failed"
