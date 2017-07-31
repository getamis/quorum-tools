module Main where

import Data.Monoid ((<>))

import QuorumTools.Test.Istanbul.RestartNonProposerTest

run :: String -> IO () -> IO ()
run description action = do
  putStrLn $ "\n" <> description <> " test"
  action

main :: IO ()
main = do
  run "restart non proposer" restartNonProposerTestMain