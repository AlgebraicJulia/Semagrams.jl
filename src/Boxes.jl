module Boxes
export Circle, Square

using ..SVG

const BOXRADIUS=40

Circle = @svg circle(r=BOXRADIUS)

const square_pts =
  [-1 -1;
    1 -1;
    1  1;
   -1  1]

Square = @svg polygon(points=svgpointstring(BOXRADIUS .* square_pts))

end
