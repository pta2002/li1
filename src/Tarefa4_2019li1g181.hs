{- | Module      : Tarefa4_2019li1g181
Description : Módulo que contém como função principal (@passo@) da Tarefa 4 
 
= Introdução

O desafio da Tarefa 4 era calcular o efeito da passagem do tempo. Esta tarefa facilmente se dividia em 
duas partes: Atualizar a velocidade do ‘Jogador’ e Movimentar o ‘Jogador’.

= Objetivos

Como mencionado na introdução, dividimos o trabalho em duas partes. Assim ficamos com dois objetivos.
Numa primeira parte tínhamos que implementar a função ‘acelera’. Esta parte era muito teórica e envolvia 
na maior parte copiar fórmulas disponíveis no enunciado. Quando estava no chão a  ‘velocidadeJogador’ era 
afetada pelo atrito das ‘Peca’s, pela sua velocidade inicial e tempo percorrido. No ar a ‘velocidadeJogador’ 
era afetada pela resistência do ar, pela velocidade inicial e tempo percorrido. A ‘gravidadeJogador’ era 
condicionada pela a gravidade inicial, pela aceleração da gravidade e pelo tempo decorrido.

Numa segunda fase, era necessário implementar a função ‘move’. Tal como a função ‘acelera’ esta também 
recorria a muitas fórmulas. No entanto, esta aborda a parte da trajetória. Para facilitar a abordagem 
deste problema, dividimos a função em três casos. Para quando estivesse no ar, no chão ou então morto. 
Quando estava no ar, era necessário saber se quando ele colidia com o chão se encontrava a mais de 45º de 
diferença de inclinação. Outro problema recorrente era também o facto, que se o ‘Jogador’ passa-se o fim da 
‘Peca’ ele passava para o início da ‘Peca’ a seguir. Logo tivemos que arranjar uma forma de saber se o 
‘Jogador’ passava o limite da ‘Peca’ e se passava ia simplesmente para o início da aseguir.

= Conclusão

Esta tarefa não foi muito complexa, mas foi muito teórica. Resumidamente, era traduzir o que estava 
detalhado no enunciado para a linguagem de programação.

-}
module Tarefa4_2019li1g181 where

import LI11920
import Tarefa2_2019li1g181 (encontraPeca,checkChao,inclinacaoPeca,alturaEmPos)
import Tarefa1_2019li1g181 (piso,gera)
import Tarefa0_2019li1g181 (Reta, Ponto(..), posx, posy, intersetam, intersecao, somaVetores, subtraiVetores, deg2rad, multiplicaVetor)
import Data.Fixed (mod')

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 =
  [ 
  -- Testar atritos
    (0.2, mapaTeste, Jogador 0 0 1 1 (Chao False))
  , (0.2, mapaTeste, Jogador 0 1 1 1 (Chao False))
  , (0.2, mapaTeste, Jogador 0 2 1 1 (Chao False))
  , (0.2, mapaTeste, Jogador 0 3 1 1 (Chao False))
  , (0.2, mapaTeste, Jogador 0 4 1 1 (Chao False))
  , (0.2, mapaTeste, Jogador 0 5 1 1 (Chao False))

  -- Testar aceleração
  , (0.2, mapaTeste, Jogador 0 0 1 1 (Chao True))

  -- Testar acabar no final da peça
  , (2, mapaTeste, Jogador 0 0 1 1 (Chao True))
  , (2, mapaTeste, Jogador 0 0 5 1 (Ar 1 0 0))

  -- Testar movimentar no ar
  , (0.2, mapaTeste, Jogador 0 0 1 1 (Ar 1 0 10))
  , (0.2, mapaTeste, Jogador 0 0 1 1 (Ar 1 (-50) 10))
  , (0.2, mapaTeste, Jogador 0 0 1 1 (Ar 1 50 10))
  , (0.2, mapaTeste, Jogador 0 0 1 1 (Ar 1 0 0))
  
  -- Testar morto
  , (0.2, mapaTeste, Jogador 0 0 0 1 (Morto 1))
  , (0.2, mapaTeste, Jogador 0 0 0 1 (Morto 0.1))
  
  --Testar movimentar no chao
  , (0.2, mapaTesteRampa, Jogador 0 0.9 2 1 (Chao False))
  , (0.2, mapaTesteRampa, Jogador 0 0 2 1 (Chao False))
  , (0.2, mapaTesteRampa, Jogador 0 1.9 2 1 (Chao False))
  , (0.2, mapaTesteRampa, Jogador 0 1 2 1 (Chao False)) 
  , (0.2, mapaTesteRampa, Jogador 1 1.5 3 1 (Chao True))
  , (2, mapaTesteRampa, Jogador 1 1.5 3 1 (Chao False))
  ]

mapaTeste =
  [ [Recta Terra 0, Recta Lama 0, Recta Cola 0, Recta Boost 0, Recta Relva 0, Recta Terra 0]
  ]
mapaTesteRampa = 
  [ [ Recta Terra 0 , Rampa Terra 0 2, Recta Terra 2, Rampa Terra 2 0]
  , [ Recta Terra 0 , Rampa Terra 0 2, Rampa Terra 2 0 , Recta Terra 0]
  ]

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
      -> Mapa    -- ^ O mapa utilizado.
      -> Jogador -- ^ O estado anterior do 'Jogador'.
      -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = fst $ passo' t m j

-- | Função alternativa de 'passo' que não desperdiça tempo
passo' :: Double -> Mapa -> Jogador -> (Jogador, Double)
passo' t m j = (move t m (acelera t m j), 0)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
        -> Mapa    -- ^ O mapa utilizado.
        -> Jogador -- ^ O estado anterior do 'Jogador'.
        -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m j
    | estaMorto = j{velocidadeJogador = 0}
    | estaChao = j{velocidadeJogador = max 0 velNova}
    | otherwise = j{velocidadeJogador = max 0 velNovaAr, estadoJogador=(estadoJogador j){gravidadeJogador = gravNova}}
  where
    velNovaAr = velJogador - (resAr * velJogador * t)
    gravNova = gravJog + (accelGravidade * t)  
    gravJog = gravidadeJogador $ estadoJogador j
    estaChao = checkChao $ estadoJogador j 
    velNova = velJogador + (accelMota - atrito * velJogador) * t
    accelMota = if (velJogador < 2 && accelJogador) then 1 else 0
    velJogador = velocidadeJogador j
    estaMorto = checkMorto $ estadoJogador j 

    accelJogador = case j of
        Jogador{ estadoJogador = Chao True } -> True
        _ -> False
    atrito = case estadoJogador j of
        Chao{} -> calculaAtrito (encontraPeca j m)
        _      -> resAr
    
-- | Calcula o atrito de uma 'Peca'
calculaAtrito :: Peca   -- ^ A 'Peca'
              -> Double -- ^ O atrito
calculaAtrito p = case piso p of
    Terra -> 0.25
    Relva -> 0.75
    Lama  -> 1.5
    Boost -> (-0.5)
    Cola  -> 3

-- | A resistência do ar
resAr = 0.125
-- | A aceleração da gravidade
accelGravidade = 1.0

-- | Função alternativa de 'move' que não desperdiça tempo.
move' :: Double -> Mapa -> Jogador -> (Jogador, Double)
move' = undefined

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m j = case estadoJogador j of
    Morto{} -> detMorto t j 
    Chao{}  -> moveChao inclinacaoChao inclinacaoProxPeca xFinal alturaFinal limitePeca j -- Zé
    Ar{}    -> moveAr j pecaAtual pos vel
  where
    inclinacaoChao = inclinacaoPeca $ pecaAtual
    inclinacaoProxPeca
        | ultimaPeca = inclinacaoChao
        | otherwise = inclinacaoPeca proxPeca 
    xInicial = distanciaJogador j
    velX = (velocidadeJogador j) * (cos $ deg2rad $ inclinacaoChao) 
    yInicial = alturaJogador $ estadoJogador j  -- ! Só válido quando o jogador se encontra no ar
    velY = gravidadeJogador $ estadoJogador j   -- ! Só válido quando o jogador se encontra no ar

    pos = Cartesiano xInicial yInicial
    vel = Cartesiano (velX * t) ((-velY) * t)

    pistaAtual = m !! (pistaJogador j)
    ultimaPeca = limitePeca >= (fromIntegral $ length pistaAtual)

    limitePeca = 1 + (fromIntegral $ truncate xInicial)
    xFinal = min limitePeca (xInicial + (velX * t))
    yFinal = yInicial + velY * t
    alturaFinal = alturaEmPos (proxPeca) xFinal
    proxPeca = encontraPeca j{distanciaJogador = (xInicial + 1) } m
    pecaAtual = encontraPeca j m

-- ? E se, em vez de pararmos na proxima peça, simplesmente executarmos a move outra vez, com o
-- ? tempo que falta? Assim não ficava tão preso I guess, não faço ideia como isto vai correr a
-- ? 60fps, às tantas nem se vai notar mas ainda acho que isso era uma coisa boa para fazer.
-- ? Obviamente que não vamos poder fazer isso though, já que os testes dos professores estão feitos
-- ? para que pares quando chegas ao final da peça.

moveAr :: Jogador -> Peca -> Ponto -> Ponto -> Jogador
moveAr j peca pos vel
    | colidiu && difInclinacoes >= 45 && not inicioPeca = j { distanciaJogador = posx posFinal
                                                            , velocidadeJogador = 0
                                                            , estadoJogador = Morto 1 }
    | colidiu = j { distanciaJogador = posx posFinal
                  , estadoJogador = Chao False }
    | otherwise = j { distanciaJogador = posx posFinal
                    , estadoJogador = (estadoJogador j) { alturaJogador = posy posFinal }}
  where
    (posFinal, colidiu) = colide peca pos vel'
    difInclinacoes = abs $ (inclinacaoJogador $ estadoJogador j) - (inclinacaoPeca peca)
    vel' = ajustaVel pos vel
    inicioPeca = (distanciaJogador j) `mod'` 1 == 0

-- | Diminui o módulo da velocidade para que esta não faça com que o jogador ultrapasse o limite da
-- peça
ajustaVel :: Ponto  -- ^ A posição atual
          -> Ponto  -- ^ A velocidade atual
          -> Ponto  -- ^ A velocidade ajustada
ajustaVel p v = subtraiVetores posAjustada p
  where
    posf = somaVetores p v
    retaFinal = (Cartesiano ((rndDown $ posx p) + 1) y1, Cartesiano ((rndDown $ posx p) + 1) y2)
    y1 = (max (posy p) (posy posf)) + 1
    y2 = (min (posy p) (posy posf)) - 1
    rndDown = fromIntegral . truncate
    posAjustada
        | (truncate $ posx posf) > (truncate $ posx p) = intersecao (p, posf) retaFinal
        | otherwise = posf


-- | Calcula a posição final de um 'Ponto' após este ser translatado por um vetor, sabendo que não
-- pode acabar dentro de uma 'Peca'.
-- 
-- Devolve a posição final e se ocorreu ou não uma colisão.
colide :: Peca          -- ^ A 'Peca' a testar
       -> Ponto         -- ^ O 'Ponto' inicial
       -> Ponto         -- ^ O vetor a somar ao 'Ponto'
       -> (Ponto, Bool) -- ^ A posição final e um 'Bool' a indicar se ocorreu ou não uma colisão.
colide peca p v
    | intersetam rMove rPeca = (intersecao rMove rPeca, True)
    | otherwise = (somaVetores p v, False)
  where
    i = truncate $ posx p
    rMove = (p, somaVetores p v)
    rPeca = retaPeca (peca) i

-- | Calcula uma 'Reta' que descreve a superfície de uma 'Peca'.
retaPeca :: Peca -- ^ A 'Peca' para a qual calcular a 'Reta'
         -> Int  -- ^ O índice da 'Peca' no mapa
         -> Reta -- ^ A 'Reta' obtida
retaPeca peca i = (Cartesiano x1 y1, Cartesiano x2 y2)
  where
    x1 = fromIntegral i
    x2 = x1 + 1
    (y1,y2) = case peca of
        Recta _ h     -> (fromIntegral h, fromIntegral h)
        Rampa _ h1 h2 -> (fromIntegral h1, fromIntegral h2)

-- * Funções relativas ao caso 'Morto' da função 'move'

-- | Determina se no intervalo de tempo o 'Jogador' ainda está morto e se estiver decrementa um tempo
detMorto :: Double  -- ^ O tempo decorrido.
         -> Jogador -- ^ O estado antigo do 'Jogador'.
         -> Jogador -- ^ O estado atualizado do 'Jogador'. 
detMorto t j
    | timeOut - t > 0 = j{estadoJogador = Morto (timeOut - t)}
    | otherwise = j {velocidadeJogador = 0, estadoJogador = Chao False} 
  where
    timeOut = timeoutJogador $ estadoJogador j

-- * Funções relativas ao caso 'Chao' na função 'move' 
moveChao :: Double  -- ^ A inclinação da peça atual.
         -> Double  -- ^ A inclinação da próxima peça.
         -> Double  -- ^ A posição final.
         -> Double  -- ^ A altura final.
         -> Double  -- ^ O limite da peça atual.
         -> Jogador -- ^ O 'Jogador' a ser avaliado.
         -> Jogador
moveChao inc1 inc2 x alt limPeca j 
    | inc2 < inc1 && (limPeca == x) = j { distanciaJogador = x
                                        , estadoJogador = Ar { inclinacaoJogador = inc1
                                                             , gravidadeJogador = 0
                                                             , alturaJogador = alt + 0.01}} 
    | otherwise = j{distanciaJogador = x}

checkMorto :: EstadoJogador -- ^ O 'EstadoJogador' a verificar 
           -> Bool          -- ^ O resultado
checkMorto Morto{} = True
checkMorto _ = False