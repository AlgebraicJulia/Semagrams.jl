"""
This is a Julia translation of the schema expected by typescript.

Calling [`to_json`](@ref) on a SemagramSchema should give a schema that
typescript can understand.

There is also a macro here for creating a SemagramSchema: [`@semagramschema`](@ref),
which makes writing down a schema much more convenient.
"""
module Schema

export AttributeType, Numeric, Stringlike,
  BoxProperties, PortProperties, WireProperties,
  SemagramSchema, BoxDesc, PortDesc, WireDesc, DataDesc,
  @semagramschema
  
import ..Muesli: to_json, from_json
using ..Muesli
using ..SVG
using Catlab.Present, Catlab.Theories
using MLStyle

@enum AttributeType begin
  Numeric
  Stringlike
end

to_json(x::AttributeType) = @match x begin
  Numeric => "Numeric"
  Stringlike => "Stringlike"
end

function from_json(d::Any, ::Type{AttributeType})
  @match d begin
    "Numeric" => Numeric
    "Stringlike" => Stringlike
  end
end

struct BoxProperties
  weights::Vector{Tuple{AttributeType, Symbol}}
  shape::String
  label::Union{Symbol,Nothing}
end

to_json(x::BoxProperties) = generic_to_json(x)
from_json(d::Dict{String,<:Any}, ::Type{BoxProperties}) = generic_from_json(d, BoxProperties)

struct PortProperties
  weights::Vector{Tuple{AttributeType, Symbol}}
  box::Symbol
  box_map::Symbol
  style::String
end

to_json(x::PortProperties) = generic_to_json(x)
from_json(d::Dict{String,<:Any}, ::Type{PortProperties}) = generic_from_json(d, PortProperties)

struct WireProperties
  weights::Vector{Tuple{AttributeType, Symbol}}
  src::Tuple{String, Symbol}
  src_map::Symbol
  tgt::Tuple{String, Symbol}
  tgt_map::Symbol
  style::String
end

to_json(x::WireProperties) = generic_to_json(x)
from_json(d::Dict{String,<:Any}, ::Type{WireProperties}) = generic_from_json(d, WireProperties)

struct SemagramSchema
  box_types::Dict{Symbol, BoxProperties}
  port_types::Dict{Symbol, PortProperties}
  wire_types::Dict{Symbol, WireProperties}
end

to_json(x::SemagramSchema) = generic_to_json(x)
from_json(d::Dict{String, <:Any}, ::Type{SemagramSchema}) = generic_from_json(d, SemagramSchema)

struct BoxDesc
  name::Symbol
  shape::SVGNode
  label::Union{Symbol,Nothing}
  function BoxDesc(name,shape=Circle,label=nothing)
    new(name,shape, label)
  end
end

struct PortDesc
  name::Symbol
  box_map::Symbol
  style::String
  function PortDesc(name,box_map,style="Circular")
    new(name,box_map,style)
  end
end

struct WireDesc
  name::Symbol
  src_map::Symbol
  tgt_map::Symbol
  style::String
  function WireDesc(name,src_map,tgt_map,style="DefaultWire")
    new(name,src_map,tgt_map,style)
  end
end

struct DataDesc
  name::Symbol
  type::AttributeType
end

const EntityDesc = Union{BoxDesc, PortDesc, WireDesc, DataDesc}

function pres_to_semagramschema(p::Presentation, descs::Array)
  ws = SemagramSchema(Dict(),Dict(),Dict())
  datadescs = filter(desc -> typeof(desc) <: DataDesc, descs)
  datatypes = Dict(map(desc -> desc.name => desc.type, datadescs)...)
  function weights(ob::Symbol)
    attrs = filter(attr -> nameof(dom(attr)) == ob, p.generators[:Attr])
    map(attr -> (get(datatypes, nameof(codom(attr)), Stringlike), nameof(attr)), attrs)
  end
  for desc in descs
    ob = desc.name
    if typeof(desc)<:BoxDesc
      ws.box_types[ob] = BoxProperties(weights(ob),write_svg(desc.shape),desc.label)
    elseif typeof(desc)<:PortDesc
      box = nameof(codom(p[desc.box_map]))
      ws.port_types[ob] = PortProperties(weights(ob),box,desc.box_map,desc.style)
    elseif typeof(desc)<:WireDesc
      src = nameof(codom(p[desc.src_map]))
      src_type = if src ∈ keys(ws.box_types)
        "Box"
      else
        "Port"
      end
      tgt = nameof(codom(p[desc.tgt_map]))
      tgt_type = if src ∈ keys(ws.box_types)
        "Box"
      else
        "Port"
      end
      ws.wire_types[ob] = WireProperties(
        weights(ob),
        (src_type, src), desc.src_map,
        (tgt_type, tgt), desc.tgt_map,
        desc.style
      )
    end
  end
  ws
end

q(x) = Expr(:quote,x)

"""
See the examples/general documentation for how to use this.
"""
macro semagramschema(head,body)
  descs = @match body begin
    Expr(:block,lines...) => begin
      map(lines) do line
        @match line begin
          Expr(:macrocall, mname, _, desc, args...) => begin
            (name,morphs) = @match desc begin
              name::Symbol => (name,[])
              Expr(:call, name, args...) => (name,args)
            end
            constructor = if mname == Symbol("@box")
              BoxDesc
            elseif mname == Symbol("@port")
              PortDesc
            elseif mname == Symbol("@wire")
              WireDesc
            elseif mname == Symbol("@data")
              DataDesc
            end
            Expr(:call, constructor, q(name), q.(morphs)..., args...)
          end
          _ => missing
        end
      end
    end
    _ => error("the body must be a block")
  end
  descs = filter(x -> !ismissing(x), descs)
  name, pres = @match head begin
    Expr(:call, name, pres) => (name, pres)
    _ => error("the head must be a name and presentation")
  end
  :($(esc(name)) = pres_to_semagramschema($(esc(pres)), $(esc(Expr(:vect, descs...)))))
end

end
