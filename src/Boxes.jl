"""
Predefined SVGs for boxes
"""
module Boxes
export Circle, SmallCircle, TinyCircle, Square

using ..SVG

const BOXRADIUS=40
const SMALLBOXRADIUS=10
const TINYBOXRADIUS=7

Circle = @svg circle(r=BOXRADIUS)

SmallCircle = @svg circle(r=SMALLBOXRADIUS)

TinyCircle = @svg circle(r=TINYBOXRADIUS)

const square_pts =
  [-1 -1;
    1 -1;
    1  1;
   -1  1]

Square = @svg polygon(points=svgpointstring(BOXRADIUS .* square_pts))

end
