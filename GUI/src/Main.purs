module Main where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)

main :: EffectFn1 { init :: Effect Unit } Unit
main = mkEffectFn1 \htmlFunctions -> do
  htmlFunctions.init
