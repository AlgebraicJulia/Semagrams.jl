module UI

export Semagram, get_acset

using JSExpr
using UUIDs
using WebIO
using ..Schema
using ..JSON
using ..Data
using Catlab.CSetDataStructures

struct Semagram{T <: AbstractACSet}
  divid::UUID
  scope::Scope
  handle::Observable{Dict{String,Any}}
  ws::SemagramSchema
  function Semagram{T}(ws::SemagramSchema) where {T <: AbstractACSet}
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

@WebIO.register_renderable(Semagram) do sg
    return WebIO.render(sg.scope)
end

function get_acset(sema::Semagram{T}) where {T}
  to_acset(from_json(sema.handle[], SemagramData), sema.ws, T)
end

end
