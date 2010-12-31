module Main where

import qualified RelationTests as RT
import qualified RelVarsTests as RVT

main :: IO ()
main = do RT.run
          RVT.run
