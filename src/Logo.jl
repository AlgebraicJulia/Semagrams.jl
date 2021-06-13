"""
The semagrams logo, rendered using our homegrown SVG library.
"""
module Logo

using ..SVG
export LOGO

julia_blue = "#4063d8"
julia_green = "#389826"
julia_red = "#cb3c33"
julia_purple = "#9558b2"

circles = [[-1,0], [0,1], [1,0], [0,-1]]
wires = [(1,1,julia_green),(1,-1,julia_red),(-1,-1,julia_purple),(-1,1,julia_blue)]
stroke_width = 0.11
r = 0.5
portr = 0.16

scale=50

offset = 1 + r + portr

function make_box(cx,cy)
  @svg(circle(r=r,cx=cx,cy=cy,style="stroke-width:$stroke_width;fill:none;stroke:black"))
end

function make_wire(x,y,color)
  src = [x, y * r]
  towards = src .+ [0, y * 0.5r]
  tgt = [x * r, y]
  from = tgt .+ [x * 0.5r, 0]
  @svg g() do
    circle(r=portr,cx=src[1],cy=src[2],style="fill:$color;stroke:none")
    circle(r=portr,cx=tgt[1],cy=tgt[2],style="fill:$color;stroke:none")
    path(d="M $(svgpointstring([src';])) C $(svgpointstring([towards';from';tgt']))",
         style="stroke-width:$stroke_width;stroke:$color;fill:none")
  end
end

LOGO = @svg svg(version="1.1", baseProfile="full",
                width=2 * offset * scale, height=2 * offset * scale,
                xmlns="http://www.w3.org/2000/svg") do
  g(transform="scale($scale $scale) translate($offset $offset)") do
    $([make_box(c...) for c in circles]...)
    $([make_wire(w...) for w in wires]...)
  end
end

end
