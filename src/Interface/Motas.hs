{-|
Module      : Interface.Motas
Description : Define Sprites para as imagens de Motas no jogo
-}                    
module Interface.Motas where

import Interface.Sprites
import Codec.Picture

colors :: [PixelRGBA8]
colors = [ PixelRGBA8 255 0   68  255
         , PixelRGBA8 18  78  137 255
         , PixelRGBA8 62  137 72  255
         , PixelRGBA8 247 118 34  255
         ]

substituiCores :: Int -> DynamicImage -> DynamicImage
substituiCores n = ImageRGBA8 . pixelMap substituiVermelho . convertRGBA8
  where
    substituiVermelho :: PixelRGBA8 -> PixelRGBA8
    substituiVermelho (PixelRGBA8 255 0 0 255) = colors !! (n `mod` length colors)
    substituiVermelho c = c

idleAnimData :: SpriteData
idleAnimData = (0.01, [(0, 0, 28, 28), (28, 0, 28, 28)])

idleAnim :: DynamicImage -> Int -> Sprite
idleAnim a i = makeSprite idleAnimData $ substituiCores i a

mortoData :: SpriteData
mortoData = (1, replicate 4 (156, 0, 28, 28))

mortoAnim :: DynamicImage -> Int -> Sprite
mortoAnim a i = makeSprite mortoData $ substituiCores i a

-- TODO Guardar sprites coloridos numa lista por jogador que é criada ao inicio do jogo.
-- Assim não há mais problemas.