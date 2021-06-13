"""
Predefined SVGs for boxes
"""
module Boxes
export Circle, SmallCircle, Square

using ..SVG

const BOXRADIUS=40
const SMALLBOXRADIUS=10

Circle = @svg circle(r=BOXRADIUS)

SmallCircle = @svg circle(r=SMALLBOXRADIUS)

const square_pts =
  [-1 -1;
    1 -1;
    1  1;
   -1  1]

Square = @svg polygon(points=svgpointstring(BOXRADIUS .* square_pts))

end
