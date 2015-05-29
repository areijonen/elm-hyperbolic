module Bounds where

type alias Bounds = { left: Float, right: Float, top: Float, bottom: Float }

fromCornerAndSize (x,y) (w,h) = { left=x, right=x+w, top=y, bottom=y+h}

width { left, right } = right-left
height { top, bottom} = top-bottom
size b = (width b, height b)
corner { left, top } = (left, top)
maxExtent b = max (width b) (height b)

