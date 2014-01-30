{-# LANGUAGE TemplateHaskell #-}
module Main where

  import Workshop.Template

  main = do
    putStrLn ( $(pr "Hello" []) )
    putStrLn ( $(pr "Hello %f" [1]) )