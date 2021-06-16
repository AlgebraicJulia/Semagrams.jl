"""
This deals with the data of a Semagram as passed back from typescript.

Basically, we recreate the data structures in typescript so that we get
automatic json conversion into a Julia-friendly format (SemagramData)
and then we provide a function to convert that into an honest-to-goodness
acset.
"""
module Data

export Box, Port, Wire, SemagramData, to_acset

using ..JSON
import ..JSON: from_json
using ..Schema
using Catlab.CSetDataStructures
using MLStyle

struct Port
  ty::Symbol
  weights::Dict{Symbol, String}
  color::Union{String, Missing}
end

from_json(d::Dict{String,Any},::Type{Port}) = generic_from_json(d,Port)

struct Box
  ty::Symbol
  weights::Dict{Symbol, String}
  ports::Dict{Int,Port}
  color::Union{String, Missing}
end

from_json(d::Dict{String,Any},::Type{Box}) = generic_from_json(d,Box)

@data Attachment begin
  AttachBox(box_idx::Int)
  AttachPort(box_idx::Int, port_idx::Int)
end

function from_json(d::Dict{String,Any},::Type{Attachment})
  if d["ty"] == "AttachBox"
    AttachBox(d["box_idx"])
  else
    AttachPort(d["box_idx"],d["port_idx"])
  end
end

struct Wire
  ty::Symbol
  weights::Dict{Symbol, String}
  src::Attachment
  tgt::Attachment
  color::Union{String, Missing}
end

from_json(d::Dict{String,Any},::Type{Wire}) = generic_from_json(d,Wire)

struct SemagramData
  boxes::Dict{Int, Box}
  wires::Dict{Int, Wire}
end

from_json(d::Dict{String,Any},::Type{SemagramData}) = generic_from_json(d,SemagramData)

function lookup_attachment(box_map::Dict{Int,Int},
                           port_map::Dict{Tuple{Int,Int},Int},
                           a::Attachment)
  @match a begin
    AttachBox(box_idx) => box_map[box_idx]
    AttachPort(box_idx,port_idx) => port_map[(box_idx,port_idx)]
  end
end

function to_acset(sd::SemagramData, schema::SemagramSchema, ::Type{T}) where {T <: AbstractACSet}
  acs = T()
  box_map = Dict{Int,Int}()
  port_map = Dict{Tuple{Int,Int},Int}()
  for (i,box) in sd.boxes
    acs_i = add_part!(acs, box.ty; box.weights...)
    box_map[i] = acs_i
    for (j,port) in box.ports
      port_props = schema.port_types[port.ty]
      acs_j = add_part!(acs,port.ty;Dict(port_props.box_map => acs_i)..., port.weights...)
      port_map[(i,j)] = acs_j
    end
  end
  for (i,wire) in sd.wires
    acs_src = lookup_attachment(box_map, port_map, wire.src)
    acs_tgt = lookup_attachment(box_map, port_map, wire.tgt)
    wire_props = schema.wire_types[wire.ty]
    attrs = Dict(wire_props.src_map => acs_src, wire_props.tgt_map => acs_tgt)
    add_part!(acs,wire.ty; attrs..., wire.weights...)
  end
  acs
end

end
