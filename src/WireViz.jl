module WireViz
export WireVizSchema, AttachType,
  BoxProperties, BoxStyle,
  PortProperties, PortStyle,
  WireProperties, WireStyle,
  decode_from_wires,
  @wirevizschema


include("SVG.jl")
include("Boxes.jl")

using Reexport

@reexport using .SVG
@reexport using .Boxes

using WebIO
using JSExpr
using UUIDs
using MLStyle
using Catlab.Theories, Catlab.Present, Catlab.CSetDataStructures

to_json(x) = x

generic_to_json(x) = Dict(string(fn)=>to_json(getfield(x,fn)) for fn ∈ fieldnames(typeof(x)))

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

struct WireVizSchema
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

function pres_to_wiresschema(p, descs)
  ws = WireVizSchema(Dict(),Dict(),Dict())
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

macro wirevizschema(head,body)
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
  :($(esc(name)) = pres_to_wiresschema($(esc(pres)), $(esc(Expr(:vect, descs...)))))
end

to_json(x::WireVizSchema) = generic_to_json(x)

struct Port
  ty::Symbol
  weights::Dict{Symbol, String}
  color::Union{String, Missing}
end

from_json(x,T) = convert(T,x) # fallback
from_json(x::String,::Type{Symbol}) = Symbol(x)

function from_json(x::Vector{Any},::Type{Vector{T}}) where {T}
  map(y -> from_json(y,T), x)
end

function from_json(x::Vector{Any},::Type{Tuple{A,B}}) where {A,B}
  (from_json(x[1],A),from_json(x[2],B))
end

function from_json(x::Vector{Any},::Type{Pair{A,B}}) where {A,B}
  from_json(x[1],A) => from_json(x[2],B)
end

function from_json(x::Vector{Any},::Type{Dict{Int,T}}) where {T}
  Dict{Int,T}(from_json(x,Vector{Pair{Int,T}})...)
end

function from_json(d::Dict{String,Any}, ::Type{Dict{Symbol,T}}) where {T}
  Dict(Symbol(k) => from_json(v,T) for (k,v) in d)
end

function generic_from_json(d::Dict{String,Any},::Type{T}) where {T}
  augmented = Dict{Symbol,Any}()
  for (i,fn) in enumerate(fieldnames(T))
    sfn = string(fn)
    if sfn ∈ keys(d)
      augmented[fn] = from_json(d[string(fn)],fieldtypes(T)[i])
    else
      augmented[fn] = missing
    end
  end
  T([augmented[fn] for fn in fieldnames(T)]...)
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

struct WireVizData
  boxes::Dict{Int, Box}
  wires::Dict{Int, Wire}
end

from_json(d::Dict{String,Any},::Type{WireVizData}) = generic_from_json(d,WireVizData)

function lookup_attachment(box_map::Dict{Int,Int},
                           port_map::Dict{Tuple{Int,Int},Int},
                           a::Attachment)
  @match a begin
    AttachBox(box_idx) => box_map[box_idx]
    AttachPort(box_idx,port_idx) => port_map[(box_idx,port_idx)]
  end
end

function to_acset(wd::WireVizData, ws::WireVizSchema, ::Type{T}) where {T <: AbstractACSet}
  acs = T()
  box_map = Dict{Int,Int}()
  port_map = Dict{Tuple{Int,Int},Int}()
  for (i,box) in wd.boxes
    acs_i = add_part!(acs,box.ty)
    box_map[i] = acs_i
    for (j,port) in box.ports
      port_props = ws.port_types[port.ty]
      acs_j = add_part!(acs,port.ty;Dict(port_props.box_map => acs_i)...)
      port_map[(i,j)] = acs_j
    end
  end
  for (i,wire) in wd.wires
    acs_src = lookup_attachment(box_map, port_map, wire.src)
    acs_tgt = lookup_attachment(box_map, port_map, wire.tgt)
    wire_props = ws.wire_types[wire.ty]
    acs_i = add_part!(acs,wire.ty;
                      Dict(wire_props.src_map => acs_src, wire_props.tgt_map => acs_tgt)...)
  end
  acs
end

struct ControlWireViz{T <: AbstractACSet}
  divid::UUID
  scope::Scope
  handle::Observable{Dict{String,Any}}
  ws::WireVizSchema
  function ControlWireViz{T}(ws::WireVizSchema) where {T <: AbstractACSet}
    divid = uuid4()
    deps = [
      "wires" => joinpath(@__DIR__, "..", "javascript", "dist", "app.bundle.js")
    ]
    scope = Scope(
      imports=deps,
      dom=dom"div"(id=string(divid))
    )
    handle = Observable(scope, "wires", Dict{String, Any}())
    ws_json = to_json(ws)
    onmount(scope, @js function()
             @var wires = System.registry.get(System.resolveSync("wires"))
             setTimeout(() -> wires.main($ws_json, $(string(divid)), (x) -> $handle[] = x), 20)
             end)
    new{T}(divid, scope, handle, ws)
  end
end

function get_acset(cw::ControlWireViz{T}) where {T}
  to_acset(from_json(cw.handle[], WireVizData),cw.ws, T)
end

end
