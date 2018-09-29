module Render where

import Prelude

import Data.Function.Uncurried (Fn1, mkFn1)
import Math as Math

deg2rad :: Fn1 Number Number
deg2rad = mkFn1 \deg ->
  deg * Math.pi / 180.0
