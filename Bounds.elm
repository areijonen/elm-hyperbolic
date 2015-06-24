module Bounds where

type alias Bounds = { left: Float, right: Float, top: Float, bottom: Float }

fromCornerAndSize (x,y) (w,h) = { left=x, right=x+w, top=y, bottom=y+h}

width { left, right } = right-left
height { top, bottom} = bottom-top
size b = (width b, height b)
corner { left, top } = (left, top)
maxExtent b = max (width b) (height b)

move { left, right, top, bottom} (x,y) = { left=left+x, right=right+x, top=top+y, bottom=bottom+y }

union b b' =
  {
    left = min b.left b'.left,
    top  = min b.top b'.top,
    right = max b.right b'.right,
    bottom = max b.bottom b'.bottom
  }
