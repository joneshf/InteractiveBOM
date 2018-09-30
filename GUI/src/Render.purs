module Render
  ( deg2rad
  , drawtext
  ) where

import Prelude

import Control.Monad.State (evalStateT, get, modify)
import Control.Monad.Trans.Class (lift)
import Data.Array (last, length, (!!))
import Data.Array.NonEmpty (NonEmptyArray, head, tail)
import Data.Foldable (elem, fold, for_, traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Function.Uncurried (Fn1, mkFn1)
import Data.Int (toNumber)
import Data.Map (lookup)
import Data.Maybe (maybe)
import Data.Newtype (wrap)
import Data.String (split, toCodePointArray)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Uncurried (EffectFn4, mkEffectFn4)
import Graphics.Canvas (Context2D, LineCap(..), beginPath, lineTo, moveTo, restore, rotate, save, scale, setFillStyle, setLineCap, setLineWidth, setStrokeStyle, stroke, translate)
import Math as Math
import PCBFont (Point, pcbFont)

type Text
  = { angle :: Number
    , attr :: Array String
    , height :: Number
    , horiz_justify :: Int
    , pos :: Array Number
    , text :: String
    , thickness :: Number
    , width :: Number
    }

calcFontPoint ::
  Text ->
  Number ->
  Number ->
  Number ->
  Point ->
  Tuple Number Number
calcFontPoint text tilt offsety offsetx { x, y } =
  -- Adding half a line height here is technically a bug
  -- but pcbnew currently does the same, text is slightly shifted.
  Tuple (point0 - (point1 + text.height * 0.5) * tilt) point1
  where
  point0 = x * text.width + offsetx
  point1 = y * text.height + offsety


deg2rad :: Fn1 Number Number
deg2rad = mkFn1 \deg ->
  deg * Math.pi / 180.0

drawtext :: EffectFn4 Context2D Text String Boolean Unit
drawtext = mkEffectFn4 \ctx text color flip ->
  for_
    ({translateX: _, translateY: _} <$> text.pos !! 0 <*> text.pos !! 1)
    \pos -> do
      save ctx
      translate ctx pos
      angle <- if "mirrored" `elem` text.attr
        then do
          scale ctx { scaleX: -1.0, scaleY: 1.0 }
          pure text.angle
        else pure (-text.angle)
      let tilt = if "italic" `elem` text.attr then 0.125 else 0.0
          interline = (text.height * 1.5 + text.thickness) / 2.0
          txt = split (wrap "\n") text.text
      rotate ctx (deg2rad angle)
      setFillStyle ctx color
      setStrokeStyle ctx color
      setLineCap ctx Round
      setLineWidth ctx text.thickness
      forWithIndex_ txt \i txti -> do
        let lineWidth = maybe 0.0 _.offsetx (last widths)
            offsety =
              toNumber (i * 2 - length txt + 1) * interline + text.height / 2.0
            widths = mkWidths text txti
            justify = case text.horiz_justify of
              -- Justify left, do nothing
              -1 -> 0.0
              -- Justify center
              0 -> lineWidth / -2.0
              -- Justify right
              1 -> -lineWidth
              -- We have to handle the 2^54 - 3 other cases.
              _ -> 0.0
        for_ widths \{ l, offsetx, w, width } -> do
          let calc = calcFontPoint text tilt offsety (justify + width)
          for_ l \line -> do
            -- Drawing each segment separately instead of
            -- polyline because round line caps don't work in joints
            beginPath ctx
            uncurry (moveTo ctx) (calc $ head line)
            traverse_ (uncurry (lineTo ctx) <<< calc) (tail line)
            stroke ctx
      restore ctx

type Width =
  { l :: Array (NonEmptyArray Point)
  , offsetx :: Number
  , w :: Number
  , width :: Number
  }

mkWidths :: forall r. { width :: Number | r } -> String -> Array Width
mkWidths text str =
  fold $ flip evalStateT 0.0 $ for (toCodePointArray str) \c -> do
    { l, w } <- lift (lookup c pcbFont.font_data)
    width <- get
    offsetx <- modify (_ + w * text.width)
    pure { l, offsetx, w, width }
