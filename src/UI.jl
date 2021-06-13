"""
This is the module where the actual interaction with typescript happens.

It's mostly wrapped up in the Semagrams struct; see the documentation there.
"""
module UI

export Semagram, get_acset, serve_semagram

using JSExpr
using UUIDs
using WebIO
using Mux
import DefaultApplication
using ..Schema
using ..JSON
using ..Data
using Catlab.CSetDataStructures

"""
Basically the "control wires" for a semagram.

Contains:
- `divid`, the DOM ID of the div where the semagram should be anchored.
- `scope`, the WebIO scope in which the semagram lives
- `handle`, the WebIO handle in which data is passed *back* from the semagram.
   Note: there is currently no way of passing data *to* the semagram, but when
   we add this, it should be a parameter here.
- `ws`, the schema of the semagram. This gets passed to typescript, and also
   tells us how to interpret the data that we are getting back from typescript.
"""
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

"""
This serves a semagram in a standalone page (as opposed to having it displayed
inline in a Jupyter notebook).

TODO: I currently don't know how to kill a task spawned by this...
"""
function serve_semagram(s::Semagram; port=8000)
  task = webio_serve(page("/", req -> node(:div, s)))
  sleep(1)
  DefaultApplication.open("http://localhost:$port")
  task
end

end
