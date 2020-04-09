module Interface.Menu where

import Graphics.Gloss
import Interface.View
import Interface.Mapa
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Interface.Estado
import LI11920
import Tarefa1_2019li1g181
import Data.List (findIndex,splitAt,init)
import Data.Maybe (fromJust)

instance Eq EstadoButton where
      x == y = estadoButtonIgual x y

estadoButtonIgual :: EstadoButton -> EstadoButton -> Bool
estadoButtonIgual x y = selecionado x == selecionado y 

{-
botaoMapa :: EstadoButton
botaoMapa = EstadoButton (DesenhaMapa estadoDefault ) (40,20)  (0,0) True True

botaoNiveis :: EstadoMenu -> EstadoButton
botaoNiveis e = EstadoButton Niveis (40,20) (0,-80)  False True 

botaoMenu :: EstadoButton
botaoMenu =EstadoButton Menu  (40,20) (0,-160) False True  

butoes :: [EstadoButton]
butoes = [botaoMapa,botaoNiveis,botaoMenu] -- Se dispor de forma a que os botões estejam ordenados no eixo da vertical poupo mais trabalho e não tenho que
                                 -- que lidar com o problema caso os botões estejam na mesma ordenada
-}
fps :: Int
fps = 60


botoesToPic :: [EstadoButton] -> [Picture]
botoesToPic []  = []
botoesToPic ( b : li ) = (Translate (x) (y) pic) : (botoesToPic li)
      where
        sel = selecionado b
        (x,y) = ponto b
        (z,w) = dimensao b
        pic = imagem b
        

drawMenu :: EstadoJogo -> Picture
drawMenu e = Pictures  ((color (makeColor (18/255) (78/255) (137/255) 1) $ rectangleSolid 100000 10000):(head li):(botoesToPic bt)) -----
      where
           bt = botoes e
           li = imagens e
           

eventMenu :: Event -> EstadoJogo -> IO EstadoJogo
eventMenu (EventKey (MouseButton LeftButton)  Down _ p ) e = instanciaToEstado  (checkButton p (botoes e)) e
eventMenu (EventKey (SpecialKey KeyDown) Down _ _ ) e      = return $ e{botoes = mudarSelec (-1) (botoes e)}
eventMenu (EventKey (SpecialKey KeyUp) Down _ _ ) e        = return $ e{botoes = mudarSelec 1 (botoes e)}
eventMenu (EventKey (SpecialKey KeyEnter) Down _ _) e      = instanciaToEstado (verSelec (botoes e)) e 
eventMenu _ e = return e

instanciaToEstado :: Maybe Instancia -> EstadoJogo -> IO EstadoJogo
instanciaToEstado x e = case x of
        Nothing -> return e
        Just Niveis -> return e{ botoes = [],imagens = [Circle 4]} 
        _ -> mapaAleatorio

checkButton :: Point           -- ^ As coordenadas do cursor que pressiona
            -> [EstadoButton]  -- ^ O Botão a testar
            -> Maybe Instancia -- ^ O resultado se o cursor está em cima do botão
checkButton _ [] = Nothing
checkButton (x,y) ( b : li) 
          |x <= xmax && x >= xmin && y <= ymax && y >= ymin = Just i 
          |otherwise = checkButton (x,y) li
    where
      i = instancia b
      d = dimensao b
      point = ponto b
      coordDim = dimencaoToCoordenada point d
      coordMin = fst coordDim
      coordMax = snd coordDim
      xmin = fst coordMin
      xmax = fst coordMax
      ymin = snd coordMin
      ymax = snd coordMax 
dimencaoToCoordenada :: Point-> (Float,Float) -> (Point,Point)
dimencaoToCoordenada (x,y) (a,b) =( (x - (a/2) , y - (b/2) ) , (x + (a/2) , y + (b/2) ) )

verSelec :: [EstadoButton] -> Maybe Instancia
verSelec [] = Nothing
verSelec (b : li)
      | selecionado b = Just (instancia b)
      | otherwise = verSelec li  

mudarSelec :: Int -> [EstadoButton] -> [EstadoButton]
mudarSelec n l
      | n == -1 && bs == []  = l
      | n == 1 && a == [] = l
      | n == -1 = a ++ [mudarImagemSel b{selecionado = False}] ++ [mudarImagemSel (head bs){selecionado = True} ] ++ (tail bs)  
      | otherwise = init a ++ [mudarImagemSel (last a){selecionado = True}] ++ [mudarImagemSel  b {selecionado = False} ] ++ bs
     where
         x = fromJust $ findIndex selecionado l
         (a,b:bs) = splitAt x l

mudarImagemSel :: EstadoButton -> EstadoButton
mudarImagemSel e = e {imagem = picSel, imagemSel = pic} 
      where
            pic = imagem e
            picSel = imagemSel e

updateMenu :: Float -> EstadoJogo -> EstadoJogo
updateMenu _ e = e
