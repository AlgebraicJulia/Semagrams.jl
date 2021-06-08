module Wires

using ..SVG

spacing = 0.5
zags = 6
resistor_internal = vcat([[x, (x % 2 == 0) ? 1 : -1]' for x in 1:zags]...)
scale = 15
resistor_pts = vcat([0              0;
                     2              0],
                    [2              0] .+ resistor_internal,
                    [(2 + zags + 1) 0;
                     (2 + zags + 3) 0]) .* [spacing 1] .* scale

Resistor = @svg g(transform="translate(0 $scale)") do
  polyline(points=svgpointstring(resistor_pts),
           style="stroke-width:1.5px;stroke:black;fill:none")
end

end
