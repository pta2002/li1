{-|
Module      : Main
Description : Este módulo define o jogo em si

= Introdução
Esta tarefa consiste em implementar o jogo em si, utilizando o código
desenvolvido nas tarefas anteriores e a biblioteca Gloss para criar uma
interface gráfica para o jogo.

= Conteudo
Quisemos criar um jogo simples que fosse no entanto divertido e completo, com
gráficos mais modernos que os do jogo original da NES.

Para isso, decidimos implementar gráficos 3D para o mapa, mantendo no entanto
os gráficos /pixel-art/ presentes no jogo original.

Também quisemos criar vários níveis selecionáveis pelo jogador e implementar ainda
funcionalidades multijogador, mas isto foi impossivel devido à falta de tempo.

= Conclusão e discussáo
Gostamos do resultado final do nosso jogo, apesar de, devido a limitações do
gloss, não ter a melhor performance.

Depois de fazer este projeto, percebemos que o Haskell não é uma linguagem
ideal para o desenvolvimento de jogos, devido ao seu estilo funcional. Mais
do que isso, achámos o Gloss bastante limitado, devido a limitações no que
conta a manipulação de imagens e a falta de muitas funcionalidades presentes
em bibliotecas de jogos para outras linguagens de programação, como por
exemplo funcionalidades de áudio, suporte para comandos de consola e a
habilidade para escrever texto utilizando fontes.
-}

module Main where

import LI11920
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Interface.Menu
import Interface.Mapa
import Interface.Final
import Interface.Estado

fps :: Int
fps = 60

dm :: Display
dm = InWindow "ExciteBike" (800, 600) (0, 0) --FullScreen

drawEstado :: EstadoJogo -> IO Picture
drawEstado e@EstadoMenu{} = return $ drawMenu e
drawEstado e@EstadoJogo{} = return $ drawMapa e
drawEstado e@EstadoFinal{} = return $ drawFinal e

updateEstado :: Float -> EstadoJogo -> IO EstadoJogo
updateEstado t e@EstadoMenu{} = return $ updateMenu t e
updateEstado t e@EstadoJogo{} = return $ updateMapa t e
updateEstado t e@EstadoFinal{} = return $ updateFinal t e

eventEstado :: Event -> EstadoJogo -> IO EstadoJogo
eventEstado ev e@EstadoMenu{} = eventMenu ev e
eventEstado ev e@EstadoJogo{} = return $ eventMapa ev e
eventEstado ev e@EstadoFinal{} = eventFinal ev e

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
main :: IO ()
main = do
    ei <- estadoInicialMenu
    -- let ei = EstadoFinal 2 undefined

    playIO dm (greyN 0.5) 60 ei
           drawEstado eventEstado updateEstado
