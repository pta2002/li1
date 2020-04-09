module Interface.Mapa where

import Graphics.Gloss
import Data.Map (Map, (!))
import qualified Data.Map as M
import LI11920
import Tarefa4_2019li1g181
import Tarefa2_2019li1g181
import Tarefa1_2019li1g181
import Tarefa0_2019li1g181
import Tarefa6_2019li1g181
import Interface.ThreeD
import Interface.View
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import Codec.Picture
import Interface.Motas
import Interface.Sprites
import Data.List
import Interface.Estado
import System.Random

-- TODO usar eventos para alterar X

mapa :: Mapa -> [Jogador] -> IO EstadoJogo
mapa m js = do
    spritesheet <- fromRight undefined <$> readImage "res/sprites.png"

    return (estadoInicialMapa m js spritesheet)

mapaAleatorio :: IO EstadoJogo
mapaAleatorio = do
    seed <- randomIO
    let m = gera 4 100 seed
    mapa m [Jogador 0 0 0 5 (Chao False), Jogador 1 0 0 5 (Chao False), Jogador 2 0 0 5 (Chao False), Jogador 3 0 0 5 (Chao False)]

updateMapa :: Float -> EstadoJogo -> EstadoJogo
updateMapa t e@EstadoJogo{ cameraX = x, sprsIdle = s, mapaJogo = m, jogadoresJogo = j}
    | botTimer e == 0 = correBot ne
    | otherwise = ne
  where
    ntimer = if botTimer e + t >= 1 then 0
                                    else botTimer e + t 
    nJogadores = passoSeNaoAcabou e (realToFrac t) <$> enumerate j
    ne = verificaAcabados $ e { cameraX = realToFrac $ (-1) * (distanciaJogador (nJogadores !! 0))
                              , sprsIdle = updateSprite t <$> s
                              , jogadoresJogo = nJogadores
                              , botTimer = ntimer
                              }

verificaAcabados :: EstadoJogo -> EstadoJogo
verificaAcabados e@EstadoJogo{jogadoresJogo=js, sprsIdle = s}
    | elem 0 jogadoresAcabados = EstadoFinal{posicao = length jogadoresAcabados, spr = s !! 0}
    | otherwise = e{jogadoresAcabados = jogadoresAcabados}
  where
    comprimentoPista = length $ head $ mapaJogo e
    jogadoresAcabados = map snd $ filter (\(j,_) -> acabou j) $ enumerate $ js
    acabou (Jogador{distanciaJogador = i}) = (truncate i) >= comprimentoPista 

passoSeNaoAcabou :: EstadoJogo -> Double -> (Jogador, Int) -> Jogador
passoSeNaoAcabou e@EstadoJogo{jogadoresAcabados = as, mapaJogo = m} t (j, i)
    | elem i as = j
    | otherwise = passo t m j

-- | Executa todos os bots no jogo
correBot :: EstadoJogo -> EstadoJogo
correBot e@EstadoJogo{ jogadoresAcabados = as } = correEstado e $ passosBot 1
  where
    passosBot :: Int -> Estado -> Estado
    passosBot i e@Estado{ jogadoresEstado = js }
        | i == length js || elem i as = e
        | otherwise = case bot i (passosBot (i+1) e) of
            Nothing -> e
            Just j -> jogada i j (passosBot (i+1) e)
correBot e = e

incJogador :: Mapa -> Jogador -> Float
incJogador m (Jogador{estadoJogador=Ar{inclinacaoJogador=i}}) = (-1) * realToFrac i
incJogador m Jogador{distanciaJogador = x, pistaJogador = pista} = realToFrac $ (-1) * inclinacaoPeca (m !! pista !! (min (truncate x) (length (m !! pista) - 1)))

eventMapa :: Event -> EstadoJogo -> EstadoJogo
eventMapa (EventKey (SpecialKey KeyDown) Down _ _) e = jogadaT 0 (Movimenta C) e
eventMapa (EventKey (SpecialKey KeyUp) Down _ _) e = jogadaT 0 (Movimenta B) e
eventMapa (EventKey (SpecialKey KeyLeft) Down _ _) e = jogadaT 0 (Movimenta E) e
eventMapa (EventKey (SpecialKey KeyRight) Down _ _) e = jogadaT 0 (Movimenta D) e
eventMapa (EventKey (Char 'z') Down _ _) e = jogadaT 0 Acelera e
eventMapa (EventKey (Char 'z') Up _ _) e = jogadaT 0 Desacelera e
eventMapa (EventKey (Char 'x') Down _ _) e = jogadaT 0 Dispara e
eventMapa _ e = e -- Caso contrário, não se faz nada

jogadaT :: Int -> Jogada -> EstadoJogo -> EstadoJogo
jogadaT i j e@EstadoJogo{jogadoresAcabados = as}
    | elem i as = e
    | otherwise = correEstado e $ jogada i j

-- | Corre uma função que requer um 'Estado' utilizando um 'EstadoJogo'
correEstado :: EstadoJogo -> (Estado -> Estado) -> EstadoJogo
correEstado e@EstadoJogo{jogadoresJogo = js, mapaJogo = m} f = e{jogadoresJogo = njs, mapaJogo = nm}
  where (Estado nm njs) = f (Estado m js)

drawMapa :: EstadoJogo -> Picture
drawMapa e@EstadoJogo { objetoMapa = o, cameraX = x} = translate 0 (64 * 3) $ scale 64 64 $ pictures [drawObjeto (x, -5, 0) $ inserirJogadores e]

inserirJogadores :: EstadoJogo -> Objeto
inserirJogadores e@EstadoJogo{mapaJogo = m, objetoMapa = o, jogadoresJogo = j} = Combinado $ zipWith join pistas iJogadores
  where
    (Combinado pistas) = (mapaToObjeto m)
    jogadoresPorPista = insereVazios (length m) (enumerate j)
    comparaPistas a b = compare (pistaJogador b) (pistaJogador a)
    picJogador j i = Imagem (coordJogador m j) $ sprJogador e j i
    iJogadores = reverse $ Combinado <$> (map (\(j, i) -> picJogador j i)) <$> jogadoresPorPista
    join as bs = Combinado [as,bs]

-- | Enumera os elementos de uma lista
enumerate :: [a] -> [(a, Int)]
enumerate l = go 0 l
  where go i [] = []
        go i (h:t) = (h, i):go (i+1) t
  
-- | Encontra a imagem para o jogador neste momento
sprJogador :: EstadoJogo -> Jogador -> Int -> Picture
sprJogador e j i = rotate (incJogador (mapaJogo e) j) $ scale (1/24) (1/24) $ getSprite $ case estadoJogador j of
    Morto{} -> (sprsMorto e) !! i
    _ -> (sprsIdle e) !! i

insereVazios :: Int -> [(Jogador, Int)] -> [[(Jogador, Int)]]
insereVazios n l = aux (replicate n []) l
  where
    aux acc [] = acc
    aux acc ((j@Jogador{pistaJogador = i}, n):t) = aux (as ++ ((j, n):b):bs) t
      where (as,b:bs) = splitAt i acc
  
-- | Encontra as coordenadas tridimensionais de um jogador
coordJogador :: Mapa -> Jogador -> Ponto3
coordJogador mapa Jogador{pistaJogador = z, distanciaJogador = x, estadoJogador = e} = case e of
    Ar{alturaJogador = y} -> (realToFrac x, realToFrac y, fromIntegral z + 1)
    _ -> (realToFrac x, realToFrac $ altura (mapa !! z) x, fromIntegral z + 1)

altura :: Pista -> Double -> Double
altura p x = a
  where
    peca = truncate x
    a = if length p == peca then alturaEmPos (p !! (min peca $ (length p) - 1)) (x - 0.01)
                            else alturaEmPos (p !! (min peca $ (length p) - 1)) x