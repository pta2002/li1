module Interface.View where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | Representa um ecrã do jogo, com lógica distinta.
data View a = View { updateFn :: (Float -> a -> a)
                   , eventFn :: (Event -> a -> a)
                   , drawFn :: (a -> Picture)
                   , initialState :: a
                   }

runView :: Display -- ^ O 'Display' para usar
        -> Color   -- ^ A cor de fundo
        -> Int     -- ^ A framerate do jogo
        -> View a  -- ^ A 'View' a correr
        -> IO ()
runView dm c f (View up ev dr i) = play dm c f i dr ev up