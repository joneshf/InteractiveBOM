module Render
  ( deg2rad
  , drawtext
  ) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (length, (!!), (..))
import Data.Foldable (elem, for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Function.Uncurried (Fn1, mkFn1)
import Data.Int (toNumber)
import Data.Map (lookup)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (wrap)
import Data.String (split, toCodePointArray)
import Effect.Class (liftEffect)
import Effect.Ref as Effect.Ref
import Effect.Uncurried (EffectFn4, mkEffectFn4)
import Graphics.Canvas (Context2D, LineCap(..), beginPath, lineTo, moveTo, restore, rotate, save, scale, setFillStyle, setLineCap, setLineWidth, setStrokeStyle, stroke, translate)
import Math as Math
import PCBFont (pcbFont)

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
  Array Number ->
  Text ->
  Number ->
  Number ->
  Number ->
  Array Number
calcFontPoint linepoint text offsetx offsety tilt =
  fromMaybe linepoint do
    linepoint0 <- linepoint !! 0
    linepoint1 <- linepoint !! 1
    let point0 = linepoint0 * text.width + offsetx
        point1 = linepoint1 * text.height + offsety
    pure
      -- Adding half a line height here is technically a bug
      -- but pcbnew currently does the same, text is slightly shifted.
      [ point0 - (point1 + text.height * 0.5) * tilt
      , point1
      ]

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
      angle <- do
        angle <- Effect.Ref.new (-text.angle)
        when ("mirrored" `elem` text.attr) do
          scale ctx { scaleX: -1.0, scaleY: 1.0 }
          Effect.Ref.modify_ negate angle
        Effect.Ref.read angle
      let tilt = if "italic" `elem` text.attr then 0.125 else 0.0
          interline = (text.height * 1.5 + text.thickness) / 2.0
          txt = split (wrap "\n") text.text
      rotate ctx (deg2rad angle)
      setFillStyle ctx color
      setStrokeStyle ctx color
      setLineCap ctx Round
      setLineWidth ctx text.thickness
      forWithIndex_ txt \i' txti -> do
        let offsety =
              toNumber (-(length txt - 1) + i' * 2) * interline + text.height / 2.0
        lineWidth <- do
          lineWidth <- Effect.Ref.new 0.0
          void $ runMaybeT $ for_ (toCodePointArray txti) \c -> do
            { w } <- liftMaybe (lookup c pcbFont.font_data)
            liftEffect (Effect.Ref.modify (_ + w * text.width) lineWidth)
          Effect.Ref.read lineWidth
        offsetx' <- Effect.Ref.new 0.0
        -- Justify left, do nothing
        when (text.horiz_justify == -1) mempty
        -- Justify center
        when (text.horiz_justify == 0) do
          Effect.Ref.modify_ (_ - lineWidth / 2.0) offsetx'
        -- Justify right
        when (text.horiz_justify == 1) do
          Effect.Ref.modify_ (_ - lineWidth) offsetx'
        runMaybeT $ for_ (toCodePointArray txti) \c -> do
          { l, w } <- liftMaybe (lookup c pcbFont.font_data)
          for_ l \line -> do
            offsetx <- liftEffect (Effect.Ref.read offsetx')
            -- Drawing each segment separately instead of
            -- polyline because round line caps don't work in joints
            for_ (0 .. (length line - 2)) \i -> do
              linei <- liftMaybe (line !! i)
              linei1 <- liftMaybe (line !! (i + 1))
              liftEffect (beginPath ctx)
              case calcFontPoint linei text offsetx offsety tilt of
                [x, y] -> liftEffect (moveTo ctx x y)
                _ -> pure mempty
              case calcFontPoint linei1 text offsetx offsety tilt of
                [x, y] -> liftEffect (lineTo ctx x y)
                _ -> pure mempty
              liftEffect (stroke ctx)
          liftEffect (Effect.Ref.modify_ (_ + w * text.width) offsetx')
      restore ctx

liftMaybe :: forall a f. Applicative f => Maybe a -> MaybeT f a
liftMaybe x = MaybeT (pure x)
