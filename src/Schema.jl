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
using ..Boxes
using Catlab.Present, Catlab.Theories
using MLStyle
using JSExpr

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

@enum EntityType begin
  Box
  Port
  Wire
end

to_json(x::EntityType) = @match x begin
  Box => "Box"
  Port => "Port"
  Wire => "Wire"
end

function from_json(d::Any, ::Type{EntityType})
  @match d begin
    "Box" => Box
    "Port" => Port
    "Wire" => Wire
  end
end

struct OutgoingHom
  name::Symbol
  codom::Symbol
  codom_ty::EntityType
end

to_json(x::OutgoingHom) = generic_to_json(x)
from_json(d::Dict{String, <:Any}, ::Type{OutgoingHom}) = generic_from_json(d, OutgoingHom)

struct BoxProperties
  weights::Vector{Tuple{AttributeType, Symbol}}
  homs::Vector{OutgoingHom}
  shape::String
  label::Union{Symbol,Nothing}
  style_fn::String
end

to_json(x::BoxProperties) = generic_to_json(x)
from_json(d::Dict{String,<:Any}, ::Type{BoxProperties}) = generic_from_json(d, BoxProperties)

struct PortProperties
  weights::Vector{Tuple{AttributeType, Symbol}}
  homs::Vector{OutgoingHom}
  box::Symbol
  box_map::Symbol
  style::String
  style_fn::String
end

to_json(x::PortProperties) = generic_to_json(x)
from_json(d::Dict{String,<:Any}, ::Type{PortProperties}) = generic_from_json(d, PortProperties)

struct WireProperties
  weights::Vector{Tuple{AttributeType, Symbol}}
  homs::Vector{OutgoingHom}
  src::Tuple{EntityType, Symbol}
  src_map::Symbol
  tgt::Tuple{EntityType, Symbol}
  tgt_map::Symbol
  style::String
  style_fn::String
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
  style_fn::JSString
  function BoxDesc(name,shape=Circle; label=nothing, style_fn=@js _ -> $(Dict()))
    new(name,shape,label,style_fn)
  end
end

struct PortDesc
  name::Symbol
  box_map::Symbol
  style::String
  style_fn::JSString
  function PortDesc(name,box_map; style="Circular", style_fn=@js _ -> $(Dict()))
    new(name,box_map,style,style_fn)
  end
end

struct WireDesc
  name::Symbol
  src_map::Symbol
  tgt_map::Symbol
  style::String
  style_fn::JSString
  function WireDesc(name,src_map,tgt_map; style="DefaultWire", style_fn=@js _ -> $(Dict()))
    new(name,src_map,tgt_map,style,style_fn)
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

  obtypes = Dict{Symbol, EntityType}(
    desc.name =>
      if typeof(desc) <: BoxDesc
        Box
      elseif typeof(desc) <: PortDesc
        Port
      else
        Wire
      end
    for desc in descs)

  function weights(ob::Symbol)
    attrs = filter(attr -> nameof(dom(attr)) == ob, p.generators[:Attr])
    [(get(datatypes, nameof(codom(attr)), Stringlike), nameof(attr)) for attr in attrs]
  end

  function homs(ob::Symbol, excluding::Vector{Symbol})
    outgoing = filter(f -> nameof(dom(f)) == ob && !(nameof(f) ∈ excluding), p.generators[:Hom])
    [OutgoingHom(nameof(f), nameof(codom(f)), obtypes[nameof(codom(f))]) for f in outgoing]
  end

  for desc in descs
    ob = desc.name
    if typeof(desc)<:BoxDesc
      ws.box_types[ob] = BoxProperties(
        weights(ob),
        homs(ob,Symbol[]),
        write_svg(desc.shape),
        desc.label,
        string(desc.style_fn),
      )
    elseif typeof(desc)<:PortDesc
      box = nameof(codom(p[desc.box_map]))
      ws.port_types[ob] = PortProperties(
        weights(ob),
        homs(ob, [desc.box_map]),
        box,
        desc.box_map,
        desc.style,
        string(desc.style_fn),
      )
    elseif typeof(desc)<:WireDesc
      src = nameof(codom(p[desc.src_map]))
      tgt = nameof(codom(p[desc.tgt_map]))
      ws.wire_types[ob] = WireProperties(
        weights(ob),
        homs(ob, [desc.src_map, desc.tgt_map]),
        (obtypes[src], src), desc.src_map,
        (obtypes[tgt], tgt), desc.tgt_map,
        desc.style,
        string(desc.style_fn),
      )
    end
  end
  ws
end

q(x) = Expr(:quote,x)

equals_to_kw(x) = x
equals_to_kw(x::Expr) =
  if x.head == :(=)
    Expr(:kw, x.args...)
  else
    x
  end

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
            Expr(:call, constructor, q(name), q.(morphs)..., equals_to_kw.(args)...)
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
