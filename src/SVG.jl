"""
Our home-grown SVG library.

This is very janky. Basically, we have a tree structure (`SVGNode`) which
roughly corresponds to SVG (i.e., each node has attributes and children),
we have a macro (`@svg`) which converts a subset of Julia syntax into that
tree structure, and finally we overload Base.show in order to print out
`SVGNode` into actual SVG XML.

This is used to customize the look of particular semagrams. Currently, all we can customize
is the shape of boxes, but eventually we should be able to customize ports, wires, etc.

Note: Is this necessary? Should we just import SVG generated by other tools?
Are we really ever going to want to programmatically generate SVG?
"""
module SVG
export @svg, SVGNode, write_svg, svgpointstring

using MLStyle

"""
The basic data structure for storing SVG.
"""
struct SVGNode
  tag::Symbol
  attrs::Dict{Symbol,String}
  children::Vector{Union{SVGNode, String}}

  """
  Note that attributes/children can be passed in as non-strings,
  and they get automatically converted to strings.
  """
  function SVGNode(tag::Symbol, attrs::Dict,
                   children::Vector)
    new(tag,
        Dict{Symbol,String}(Symbol(k) => string(v) for (k,v) in attrs),
        to_svgnode.(children))
  end
end

function to_svgnode(x)
  string(x)
end

function to_svgnode(x::SVGNode)
  x
end

"""
This prints an SVG to a display which supports SVG (i.e., a Jupyter notebook)

If the SVG passed in is a fragment (i.e., not a full SVG), then it wraps it
with SVG tag before displaying it.

TODO: this should probably wrap with the standard attributes (i.e. xmlns)
instead of a bare SVG tag.
"""
function Base.show(io::IO, mime::MIME"image/svg+xml", svg::SVGNode)
  if svg.tag == :svg
    write_svg(io, svg)
  else
    write_svg(io, SVGNode(:svg, Dict{Symbol,Any}(), [svg]))
  end
end

function write_svg(io::IO, s::String)
  print(io, s)
end

function write_svg(io::IO, svg::SVGNode)
  print(io,"<$(string(svg.tag))")
  for (key,value) in svg.attrs
    print(io, " $(string(key))=\"$(value)\"")
  end
  print(io, ">")
  for child in svg.children
    write_svg(io, child)
  end
  print(io, "</$(string(svg.tag))>")
end

"""
Converts an SVGNode to the SVG XML syntax
"""
function write_svg(svg::SVGNode)
  sprint(write_svg, svg)
end

"""
Converts Julia syntax to a SVGNode

Example:

```julia
@svg g() do
  circle(r=30)
  circle(r=40)
end
```

For more examples of usage, see Logo.jl
"""
macro svg(block)
  esc(make_svg(block))
end

function strip_lines(expr::Expr; recurse::Bool=false)::Expr
  args = [ x for x in expr.args if !isa(x, LineNumberNode) ]
  if recurse
    args = [ isa(x, Expr) ? strip_lines(x; recurse=true) : x for x in args ]
  end
  Expr(expr.head, args...)
end

function make_svg(block)
  block = strip_lines(block, recurse=true)
  @match block begin
    Expr(:block, lines...) =>
      svg_node(Expr(:do, Expr(:call, :svg), Expr(:(->), (), Expr(:block, lines...))))
    Expr(:do, _args...) => svg_node(block)
    Expr(:call, _args...) => svg_node(block)
    _ => error("Expected a block")
  end
end

function svg_node(stx::Expr)
  (head, body) = @match stx begin
    Expr(:do, head, Expr(:(->), _, Expr(:block, lines...))) => (head, [lines...])
    _ => (stx,Expr[])
  end
  @match head begin
    Expr(:call, tag, args...) =>
      Expr(:call,
           SVGNode,
           Expr(:quote, tag),
           Expr(:call, Dict{Any,Any},
                map((e) ->
                    Expr(:call, :(=>),
                         Expr(:quote, e.args[1]),
                         e.args[2]),
                    args)...),
           Expr(:vect, map(svg_node, body)...))
    Expr(:$, expr) => expr
    _ => error("expected a function call or an escape")
  end
end

svg_node(x) = x

# Utility functions

svgpointstring(X) = join([join(X[i,:],",") for i in 1:(size(X)[1])]," ")

end