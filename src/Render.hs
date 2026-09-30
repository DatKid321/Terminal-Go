module Render where

import qualified Render.ASCII as ASCII
import qualified Render.Sixel as Sixel

import Types

data Graphics
    = ASCII
    | Sixel
    deriving (Eq, Show)

render :: Graphics -> Rules -> Position -> String
render ASCII = ASCII.render
render Sixel = Sixel.render