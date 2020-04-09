{-| 
Module : Tarefa6_2019li1g181
Description : Módulo que contém como função principal (@bot@) da Tarefa 6
       
= Introdução

Nesta tarefa o desafio que encontramos foi o de munir um ro'bot' com a capacidade de
navegar pelo 'Mapa' da forma mais eficiente. Este robot tinha que a partir do 'Estado' mais 
atualizado tomar decisão sobre que 'Jogada' iria fazer, ou então se simplesmente não fazia nada.
O resultado final foi um ro'bot' que escolhe qual o caminho mais rápido e que nunca morre.

= Objetivos 

O objetivo final era conseguir que o ro'bot' determinasse o caminho mais rápido. No entanto,
o 'Mapa' pode-se alterar devido a ações dos outros 'Jogador'es. Isto é, o 'Mapa' pode ser diferente 
desde o ponto de partida até o ponto de chegada.  Assim tivemos que avaliar como é que íamos 
determinar o melhor caminho. A nossa estratégia foi avaliar as peças da vizinhança do ro'bot' 
e comparar qual delas tinha o menor atrito. A 'Peca' que tinha o menor era para onde o ro'bot' se movia.
(Ainda quero arranjar uma forma de expandir a vizinhança para ele tomar melhores decisões).

A seguir, reparamos que era importante mudar a inclinação do ro'bot'. Isto porque, caso a diferença de
inclinação fosse maior que 45º em relação à peça que ele aterrava, o ro'bot' morria e perdia tempo.
Assim comparamos a inclinação do ro'bot' com a inclinação da 'Peca' subjacente. Assim o ro'bot' alterava 
a sua inclinação para corresponder com essa 'Peca'.

Por fim, reparamos que certas 'Jogada's resultavam na morte do ro'bot'. Assim, decidimos verificar se a
'Jogada' feita ,pelas funções acima descritas, resultava em morte. Se resultasse o ro'bot' não fazia nada,
se não fazia a 'Jogada'.

= Resultados

Por observação dos torneios que decorreram, reparamos que o ro'bot' funcionava e que muitas vezes ia pelo
caminho mais curto, mas que nem sempre ia pelo caminho mais curto. Ou seja, tinha uma "visão" limitada.
Devido a este facto o ro'bot' tendia a perder contra os outros 'Jogador'es que conseguiam "ver" mais à frente.

= Conclusão

Como resultado final, temos um ro'bot' que consegue “tomar” autonomamente decisões e que consegue navegar
de forma eficiente até à chegada. No entanto, existe ainda margem para melhorar. O ro'bot' pode melhorar 
na decisão do melhor caminho. 

-}

module Tarefa6_2019li1g181 where

import LI11920
import Tarefa4_2019li1g181(calculaAtrito, checkMorto)
import Tarefa2_2019li1g181 (jogada, inclinacaoPeca, encontraPeca)
import Tarefa1_2019li1g181
import Data.List (transpose,last,head,sortOn)

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n e = case estadoRobot of
        Chao True->  proxJogadaMorto ( caminhoMenosAtrito pistaRobot (atritoPecasRaio1 posRobot pistaRobot mapa) ) robot mapa
        Morto{} -> Nothing
        Ar{}-> proxJogadaMorto ( mudaInclinacao  mapa robot ) robot mapa
        _ -> Just Acelera
    where
        jogadores =  jogadoresEstado e
        mapa =  mapaEstado e
        robot = jogadores !! n
        velocRobot = velocidadeJogador robot
        posRobot = truncate (distanciaJogador robot) 
        pistaRobot = pistaJogador robot
        estadoRobot = estadoJogador robot

atritoPecasRaio1 :: Int            -- ^ A posição do ro'bot'.
               -> Int            -- ^ A 'Pista' em que o ro'bot' se encontra.
               -> Mapa           -- ^ O 'Mapa' a avaliar.
               -> Maybe [Double] -- ^ A lista do atrito da próxima coluna de 'Peca's colineares.
atritoPecasRaio1 pos pista m
    | (pista == 0) = Just (b:[head bs])
    | (pista + 1) == length m = Just (last a : [b]) 
    | length (m!!0) > pos + 1 && length m > 3  = Just ( (last a) : b : [head bs]  )
    | length (m!!0) == pos + 1 =  Nothing -- Caso seja a última 'Peca' ele devolve Nothing
    | otherwise = Just transposta
        where
            transposta = calculaAtrito <$> (transpose m) !! (pos)
            (a,b:bs) = splitAt pista transposta

caminhoMenosAtrito :: Int                -- ^ A 'Pista' em que o ro'bot' se encontra
                   -> Maybe [Double]     -- ^ A lista do atrito da próxima coluna de 'Pecas's colineares
                   -> Maybe Jogada       -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot' .
caminhoMenosAtrito _ Nothing = Just Acelera -- Este caso é para quando é a última 'Peca' e única 'Jogada' a fazer é a 'Acelera'r.
caminhoMenosAtrito pista (Just l) = case i of 
                0 -> if (pista == 0) then Nothing else Just (Movimenta C) 
                1 -> if (pista == 0 ) then Just (Movimenta B) else Nothing ------------Tenho que mudar
                2 -> Just (Movimenta B)
    where
        w = zip [0,1,2] l
        (i,atrito) = head (sortOn snd w)
        
mudaInclinacao :: Mapa          -- ^ O 'Mapa' a avaliar.
               -> Jogador       -- ^ O ro'bot' a ser testado
               -> Maybe Jogada  -- ^ Uma possível 'Jogada' 
mudaInclinacao m robot
    | robotInclinacao < inclinacao = Just (Movimenta E)
    | robotInclinacao == inclinacao = Nothing
    | otherwise = Just (Movimenta D) 
    where
        eRobot = estadoJogador robot 
        robotInclinacao = inclinacaoJogador eRobot
        inclinacao = inclinacaoPeca $ encontraPeca robot m 

proxJogadaMorto :: Maybe Jogada -- ^ A 'Jogada' que se pretende testar.
                -> Jogador      -- ^ O ro'bot' que vai ser testado
                -> Mapa         -- ^ O 'Mapa' a ser testado
                -> Maybe Jogada -- ^ A 'Jogada' avaliada
proxJogadaMorto Nothing _ _  = Nothing
proxJogadaMorto (Just j) robot m = if (checkMorto eRobot) then Nothing else (Just j)
    where
        e = jogada 0 j Estado{ mapaEstado = m, jogadoresEstado = [robot]}
        [robotAtualizado] = jogadoresEstado e
        eRobot = estadoJogador robotAtualizado
        

