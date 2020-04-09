module Interface.ThreeD where

import Graphics.Gloss
import Tarefa1_2019li1g181 (piso)
import LI11920

-- | Representa um ponto tridimensional, utilziado para desenhar a imagem
type Ponto3 = (Float, Float, Float)

-- | Soma dois `Ponto3`
--
-- >>> (0,0,0) `add3` (2,3,5)
-- (2,3,5)
add3 :: Ponto3 -> Ponto3 -> Ponto3
add3 (x,y,z) (w,i,k) = (x+w,y+i,z+k)

rotateX :: Float -> Ponto3 -> Ponto3
rotateX a (x, y, z) = (x, (y * cos a) - (z * sin a), (y * sin a) + (z * cos a))

rotateY :: Float -> Ponto3 -> Ponto3
rotateY a (x, y, z) = ((x * cos a) + (z * sin a), y, (z * cos a) - (x * sin a))

rotateZ :: Float -> Ponto3 -> Ponto3
rotateZ a (x, y, z) = ((x * cos a) - (y * sin a), (x * sin a) + (y * cos a), z)

-- | Representa um objeto tridimensional
data Objeto = Forma Color [Ponto3]
            | LineO Color [Ponto3]
            | Imagem Ponto3 Picture
            | Combinado [Objeto]

translateO :: Ponto3 -> Objeto -> Objeto
translateO p (Forma c ps) = Forma c (add3 p <$> ps)
translateO p (LineO c ps) = LineO c (add3 p <$> ps)
translateO p (Imagem ip i) = Imagem (add3 p ip) i
translateO p (Combinado os) = Combinado (translateO p <$> os)

scale3 :: Float -> Float -> Float -> Objeto -> Objeto
scale3 x y z (Forma c ps) = Forma c (scaleP x y z <$> ps)
scale3 x y z (LineO c ps) = LineO c (scaleP x y z <$> ps)
scale3 x y z (Imagem ip i) = Imagem ip i
scale3 x y z (Combinado os) = Combinado (scale3 x y z <$> os)

scaleP :: Float -> Float -> Float -> Ponto3 -> Ponto3
scaleP x y z (a,b,c) = (a*x, b*y, z*c)

-- | Projeta o ponto num plano 2d utilizando uma projeção ortogonal.
-- https://en.wikipedia.org/wiki/Orthographic_projection
projetarPonto :: Ponto3 -> Ponto3 -> Point
projetarPonto p1 p = (x * w, y * w)
  where
    (x, y, z) = add3 p1 $ rotateX (-pi / 4) p
    w = r / (r + z)
    r = 100

-- | Converte um `Objeto` numa `Picture` que pode ser desenhada pelo Gloss.
drawObjeto :: Ponto3 -> Objeto -> Picture
drawObjeto p (Forma c ps) = color c $ polygon $ projetarPonto p <$> ps
drawObjeto p (LineO c ps) = color c $ line $ projetarPonto p <$> ps
drawObjeto p (Combinado os) = pictures $ drawObjeto p <$> os
drawObjeto p (Imagem p' i) = translate x y i
  where (x,y) = projetarPonto p p'

-- | Devolve a `Color` de um `Piso`
pisoColor :: Piso -> Int -> Color
pisoColor Terra = makeColorI 184 111 80
pisoColor Lama = makeColorI 63 40 50
pisoColor Cola = makeColorI 192 203 220
pisoColor Boost = makeColorI 38 43 68
pisoColor Relva = makeColorI 99 199 77

pecaToObjeto :: Peca -> Objeto
pecaToObjeto p = Combinado [chao, parede, l1, l2]
  where
    (y1, y2) = case p of
        Recta _ h     -> (fromIntegral h, fromIntegral h)
        Rampa _ y1 y2 -> (fromIntegral y1, fromIntegral y2)
    
    cor = if max y1 y2 > 0 then pisoColor (piso p) 200
                           else pisoColor (piso p) 255
    chao = Forma cor [(0, y1, 0), (1, y2, 0), (1, y2, 1), (0, y1, 1), (0, y1, 0)]
    parede = Forma (dim $ dim cor) [(0, 0, 0), (1, 0, 0), (1, y2, 0), (0, y1, 0), (0, 0, 0)]
    l1 = LineO (dim $ dim $ dim cor) [(0, y1, 0), (1, y2, 0)]
    l2 = LineO (dim $ dim $ dim cor) [(0, y1, 1), (1, y2, 1)]

pistaToObjeto :: Pista -> Objeto
pistaToObjeto l = Combinado $ reverse $ go 0 l
  where
    go :: Integer -> Pista -> [Objeto]
    go i [] = []
    go i (h:t) = translateO (fromInteger i, 0, 0) (pecaToObjeto h) : go (i + 1) t

mapaToObjeto :: Mapa -> Objeto
mapaToObjeto l = Combinado $ reverse $ go 0 $ l
  where
    go :: Integer -> Mapa -> [Objeto]
    go i [] = []
    go i (h:t) = translateO (0, 0, fromInteger i) (pistaToObjeto h) : go (i + 1) t