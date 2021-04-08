module Wires
using WebIO
using Catlab.Theories, Catlab.CSetDataStructures

@enum BoxStyle begin
  Square
  Circular
end

@enum PortStyle begin
  Input
  Output
  Circular
end

@enum WireStyle begin
  Default
end

struct WiresSchema
  schema::Presentation{Schema}
  boxes::Vector{Tuple{Symbol,BoxStyle}}
  ports::Vector{Tuple{Symbol,PortStyle}}
  wires::Vector{Tuple{Symbol,WireStyle}}
end

function decode_from_wires(wires::Dict{String,Any}, ws::WiresSchema, ::Type{T}) where
  {T <: AbstractACSet}
  boxids = Dict(box[1] => Int[] for box in ws.boxes)
  portids = Dict(port[1] => Int[] for port in ws.ports)
  wireids = Dict(wire[1] => Int[] for wire in ws.wires)
  acs = T()
  for box in wires["boxes"]
    boxty = Symbol(box["ty"])
    acs_id = add_part!(T, boxty)
    boxids[boxty]
  end
  for port in wires["ports"]
  end
end

end
