"""
This deals with the data of a Semagram as passed back from typescript.

Basically, we recreate the data structures in typescript so that we get
automatic json conversion into a Julia-friendly format (SemagramData)
and then we provide a function to convert that into an honest-to-goodness
acset.
"""
module Data

export Box, Port, Wire, SemagramData, LocatedSemagramData, to_acset

using ..Muesli
import ..Muesli: from_json, to_json
using ..Schema
using Catlab.CSetDataStructures
using Catlab.Theories
using MLStyle

@data Entity begin
  BoxEntity(box_idx::Int)
  PortEntity(box_idx::Int, port_idx::Int)
  WireEntity(wire_idx::Int)
end

const Attachment = Union{BoxEntity,PortEntity}

function from_json(d::Dict{String,<:Any},::Type{<:Entity})
  @match d["ty"] begin
    "Box" => BoxEntity(d["box_idx"])
    "Port" => PortEntity(d["box_idx"],d["port_idx"])
    "Wire" => WireEntity(d["wire_idx"])
  end
end

function to_json(a::Entity)
  @match a begin
    BoxEntity(box_idx) => Dict(:ty => "Box", :box_idx => box_idx)
    PortEntity(box_idx, port_idx) => Dict(:ty => "Port", :box_idx => box_idx, :port_idx => port_idx)
    WireEntity(wire_idx) => Dict(:ty => "Wire", :wire_idx => wire_idx)
  end
end

struct Port
  ty::Symbol
  weights::Dict{Symbol, String}
  homs::Dict{Symbol, Entity}
end

from_json(d::Dict{String,<:Any},::Type{Port}) = generic_from_json(d,Port)
to_json(p::Port) = generic_to_json(p)

struct Box
  ty::Symbol
  weights::Dict{Symbol, String}
  homs::Dict{Symbol, Entity}
  ports::Dict{Int,Port}
end

from_json(d::Dict{String,<:Any},::Type{Box}) = generic_from_json(d,Box)
to_json(b::Box) = generic_to_json(b)


struct Wire
  ty::Symbol
  weights::Dict{Symbol, String}
  homs::Dict{Symbol, Entity}
  src::Attachment
  tgt::Attachment
end

from_json(d::Dict{String,<:Any},::Type{Wire}) = generic_from_json(d,Wire)
to_json(w::Wire) = generic_to_json(w)

struct IDGen
  i::Int
end

from_json(d::Dict{String,<:Any},::Type{IDGen}) = generic_from_json(d,IDGen)
to_json(gen::IDGen) = generic_to_json(gen)

struct SemagramData
  boxes::Dict{Int, Box}
  wires::Dict{Int, Wire}
  gen::IDGen
  schema::SemagramSchema
  function SemagramData(schema::SemagramSchema)
    new(Dict{Int,Box}(), Dict{Int,Wire}(), IDGen(0), schema)
  end
  function SemagramData(boxes, wires, gen, schema)
    new(boxes, wires, gen, schema)
  end
end

from_json(d::Dict{String,<:Any},::Type{SemagramData}) = generic_from_json(d,SemagramData)
to_json(sd::SemagramData) = generic_to_json(sd)

struct LocatedSemagramData
  sg::SemagramData
  boxlocs::Dict{Int, Tuple{Float64, Float64}}
  function LocatedSemagramData(schema::SemagramSchema)
    new(SemagramData(schema), Dict{Int, Tuple{Float64, Float64}}())
  end
  function LocatedSemagramData(sg, boxlocs)
    new(sg, boxlocs)
  end
end

from_json(d::Dict{String,<:Any},::Type{LocatedSemagramData}) = generic_from_json(d,LocatedSemagramData)
to_json(ls::LocatedSemagramData) = generic_to_json(ls)

function lookup_attachment(box_map::Dict{Int,Int},
                           port_map::Dict{Tuple{Int,Int},Int},
                           a::Attachment)
  @match a begin
    BoxEntity(box_idx) => box_map[box_idx]
    PortEntity(box_idx,port_idx) => port_map[(box_idx,port_idx)]
  end
end

function attribute_type(::Type{T}, attr::Symbol) where {CD, AD, Ts, T <: AbstractACSet{CD, AD, Ts}}
  Ts.parameters[Theories.codom_num(AD, attr)]
end

function attributes_from_strings(weights::Dict{Symbol, String}, ::Type{T}) where {T <: AbstractACSet}
  types = Dict([attr => attribute_type(T, attr) for attr in keys(weights)]...)
  NamedTuple{(keys(types)...,),Tuple{values(types)...}}(
    [from_json(weights[attr],types[attr]) for attr in keys(weights)]
  )
end

function to_acset(ls::LocatedSemagramData, ::Type{T}) where {T <: AbstractACSet}
  sd = ls.sg
  schema = sd.schema
  acs = T()
  box_map = Dict{Int,Int}()
  port_map = Dict{Tuple{Int,Int},Int}()
  for (i,box) in sd.boxes
    acs_i = add_part!(acs, box.ty; attributes_from_strings(box.weights, T)...)
    box_map[i] = acs_i
    for (j,port) in box.ports
      port_props = schema.port_types[port.ty]
      acs_j = add_part!(acs,port.ty;Dict(port_props.box_map => acs_i)...,
                        attributes_from_strings(port.weights, T)...)
      port_map[(i,j)] = acs_j
    end
  end

  for (i,wire) in sd.wires
    acs_src = lookup_attachment(box_map, port_map, wire.src)
    acs_tgt = lookup_attachment(box_map, port_map, wire.tgt)
    wire_props = schema.wire_types[wire.ty]
    attrs = Dict(wire_props.src_map => acs_src, wire_props.tgt_map => acs_tgt)
    add_part!(acs,wire.ty; attrs..., attributes_from_strings(wire.weights, T)...)
  end

  for (i,box) in sd.boxes
    for (hom, e) in box.homs
      set_subpart!(acs, box_map[i], hom, lookup_attachment(box_map, port_map, e))
    end
    for (j,port) in box.ports
      for (hom, e) in port.homs
        set_subpart!(acs, port_map[(i,j)], hom, lookup_attachment(box_map, port_map, e))
      end
    end
  end
  acs
end

end
