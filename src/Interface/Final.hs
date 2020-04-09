-- | O ecrã final do jogo
module Interface.Final where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Interface.Estado

eventFinal (EventKey _ Down _ _) _ = estadoInicialMenu
eventFinal ev e = return e

updateFinal _ e = e

pos = [ "Primeiro", "Segundo", "Terceiro", "Quarto", "Quinto", "Sexto" ]

drawFinal EstadoFinal{posicao = n} = pictures [ color (makeColor (18/255) (78/255) (137/255) 1) $ rectangleSolid 10000 10000
                       , translate (-100) (50) $ scale 0.25 0.25 $ color (makeColor 255 255 255 255) $ Text "Game Over"
                       , translate (-200) (-10) $ scale 0.5 0.5 $ color (makeColor 255 255 255 255) $ Text (pos !! n ++ " Lugar")
                       , translate (-100) (-50) $ scale 0.1 0.1 $ color (makeColor 255 255 255 255) $ Text "Qualquer tecla para reiniciar"
                       ]