-- Вспомогательные функции для графики
module Utils where

import Graphics.Gloss

-- Прямоугольник с сильно скруглёнными углами
roundedRect :: Float -> Float -> Float -> Picture
roundedRect w h r = pictures
    [ translate (-w/2 + r) (-h/2 + r) $ circleSolid r
    , translate ( w/2 - r) (-h/2 + r) $ circleSolid r
    , translate (-w/2 + r) ( h/2 - r) $ circleSolid r
    , translate ( w/2 - r) ( h/2 - r) $ circleSolid r
    , translate 0 (-h/2 + r) $ rectangleSolid (w - 2*r) (2*r)
    , translate 0 ( h/2 - r) $ rectangleSolid (w - 2*r) (2*r)
    , translate (-w/2 + r) 0 $ rectangleSolid (2*r) (h - 2*r)
    , translate ( w/2 - r) 0 $ rectangleSolid (2*r) (h - 2*r)
    , rectangleSolid w h
    , translate (-w/2 + r/2) (-h/2 + r/2) $ circleSolid (r/2)
    , translate ( w/2 - r/2) (-h/2 + r/2) $ circleSolid (r/2)
    , translate (-w/2 + r/2) ( h/2 - r/2) $ circleSolid (r/2)
    , translate ( w/2 - r/2) ( h/2 - r/2) $ circleSolid (r/2)
    ]
