module Main (main) where

import Criterion.Main
import Data.SemVer

main :: IO ()
main =
  defaultMain
    [ bgroup
        "parseSemVer"
        [ bench "1.0.0" $ whnf parseSemVer "1.0.0"
        , bench "1.0.0-alpha1" $ whnf parseSemVer "1.0.0-alpha1"
        , bench "1.0.0-alpha+21AF26D3" $ whnf parseSemVer "1.0.0-alpha1+21AF26D3"
        ]
    ]
