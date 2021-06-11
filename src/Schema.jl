module Schema

export BoxProperties, PortProperties, WireProperties,
  SemagramSchema, BoxDesc, PortDesc, WireDesc,
  @semagramschema
  

using ..JSON
import ..JSON: to_json

struct BoxProperties
  weights::Vector{Symbol}
  shape::String
  label::String
end

to_json(x::BoxProperties) = generic_to_json(x)

struct PortProperties
  weights::Vector{Symbol}
  box::Symbol
  box_map::Symbol
  style::String
end

to_json(x::PortProperties) = generic_to_json(x)

struct WireProperties
  weights::Vector{Symbol}
  src::Tuple{String, Symbol}
  src_map::Symbol
  tgt::Tuple{String, Symbol}
  tgt_map::Symbol
  style::String
end

to_json(x::WireProperties) = generic_to_json(x)

struct SemagramSchema
  box_types::Dict{Symbol, BoxProperties}
  port_types::Dict{Symbol, PortProperties}
  wire_types::Dict{Symbol, WireProperties}
end

struct BoxDesc
  name::Symbol
  shape::SVGNode
  function BoxDesc(name,shape=Circle)
    new(name,shape)
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

function pres_to_semagramschema(p, descs)
  ws = SemagramSchema(Dict(),Dict(),Dict())
  for desc in descs
    if typeof(desc)<:BoxDesc
      ws.box_types[desc.name] = BoxProperties([],write_svg(desc.shape),string(desc.name))
    elseif typeof(desc)<:PortDesc
      box = nameof(codom(p[desc.box_map]))
      ws.port_types[desc.name] = PortProperties([],box,desc.box_map,desc.style)
    elseif typeof(desc)<:WireDesc
      src = nameof(codom(p[desc.src_map]))
      src_type = if src ∈ keys(ws.box_types)
        "AttachBox"
      else
        "AttachPort"
      end
      tgt = nameof(codom(p[desc.tgt_map]))
      tgt_type = if src ∈ keys(ws.box_types)
        "AttachBox"
      else
        "AttachPort"
      end
      ws.wire_types[desc.name] = WireProperties([],(src_type, src),desc.src_map,(tgt_type, tgt),desc.tgt_map,desc.style)
    end
  end
  ws
end

q(x) = Expr(:quote,x)

macro semagramschema(head,body)
  descs = @match body begin
    Expr(:block,lines...) => map(lines) do line
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
          end
          Expr(:call, constructor, q(name), q.(morphs)..., args...)
        end
        _ => missing
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

to_json(x::SemagramSchema) = generic_to_json(x)

end
