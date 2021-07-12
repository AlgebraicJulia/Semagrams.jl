"""
This is the module where the actual interaction with typescript happens.

It's mostly wrapped up in the Semagrams struct; see the documentation there.
"""
module UI

export Semagram, get_acset, serve_semagram, save, load

using JSExpr
using WebIO
using Mux
import JSON
import DefaultApplication
using ..Schema
using ..Muesli
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
  scope::Scope
  receiving::Observable{Dict{String,Any}}
  sending::Observable{Dict{String,Any}}
  function Semagram{T}(ls::LocatedSemagramData) where {T <: AbstractACSet}
    deps = [
      "semagrams" => joinpath(@__DIR__, "..", "deps", "bundles", "app.bundle.js")
    ]
    scope = Scope(imports=deps, dom=dom"div"())
    ls_json = to_json(ls)
    receiving = Observable(scope, "receiving", ls_json)
    sending = Observable(scope, "sending", ls_json)
    onjs(sending, @js function (newls)
           console.log(this)
           this.state.resetWith(newls)
         end)
    on((newls) -> receiving[] = newls, scope, "sending")
    mountfn = @js function ()
      @var semagrams = System.registry.get(System.resolveSync("semagrams"))
      @var scopeobj = this
      setTimeout(() ->
        semagrams.main($receiving[], scopeobj, (x) -> $receiving[] = x), 20)
    end
    onmount(scope, mountfn)
    new{T}(scope, receiving, sending)
  end
end

function Semagram{T}(s::SemagramSchema) where {T <: AbstractACSet}
  ls = LocatedSemagramData(s)
  Semagram{T}(ls)
end

@WebIO.register_renderable(Semagram) do sg
    return WebIO.render(sg.scope)
end

function get_acset(sema::Semagram{T}) where {T}
  to_acset(save(sema), T)
end

function save(sema::Semagram)
  from_json(sema.receiving[], LocatedSemagramData)
end

function save(sema::Semagram, fn::String)
  open(fn, "w") do f
    write(f, JSON.json(sema.receiving[]))
  end
end

function load(sema::Semagram, fn::String)
  load(sema, load(fn))
end

function load(sema::Semagram, ls::LocatedSemagramData)
  sema.sending[] = to_json(ls)
end

function load(fn::String)
  v = Dict()
  open(fn, "r") do f
    v = JSON.parse(read(f, String))
  end
  from_json(v, LocatedSemagramData)
end
  
"""
This serves a semagram in a standalone page (as opposed to having it displayed
inline in a Jupyter notebook).

TODO: I currently don't know how to kill a task spawned by this...
"""
function serve_semagram(s::Semagram; port=8000)
  task = webio_serve(page("/", req -> node(:div, s)), port)
  sleep(1)
  DefaultApplication.open("http://localhost:$port")
  task
end

end
