module Bounds exposing(..)

type alias Bounds = { left: Float, right: Float, top: Float, bottom: Float }

fromCornerAndSize (x,y) (w,h) = { left=x, right=x+w, top=y, bottom=y+h}

fromCenterAndExtents (cx,cy) (ex,ey) = { left=cx-ex, right=cx+ex, top=cy+ey, bottom=cy-ey }

width { left, right } = right-left
height { top, bottom} = bottom-top
size b = (width b, height b)
corner { left, top } = (left, top)
maxExtent b = max (width b) (height b)

move { left, right, top, bottom} (x,y) = { left=left+x, right=right+x, top=top+y, bottom=bottom+y }

union b b_ =
  {
    left = min b.left b_.left,
    top  = min b.top b_.top,
    right = max b.right b_.right,
    bottom = max b.bottom b_.bottom
  }
