module Main where

import Run
import Parse

main :: IO ()
main = runAction =<< prompt
