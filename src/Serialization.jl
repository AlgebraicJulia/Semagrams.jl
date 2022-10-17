module Serialization
export read_acset

using Catlab.CSetDataStructures
import JSON3

Base.lowercase(s::Symbol) = Symbol(lowercase(string(s)))

function read_acset(name::String, ::Type{T}, json::JSON3.Object, deserializers::Dict{Symbol, Function}) where {T <: ACSet}
  acs = T()
  s = acset_schema(acs)
  ob_maps = Dict{Symbol, Dict{Int, Int}}()
  for ob in ob(s)
    ob_maps[ob] = Dict{Int,Int}()
    for i in json.parts[ob]
      ob_maps[ob][i] = add_part!(acs, ob)
    end
  end

  props = Dict{Int, Dict{Symbol, Any}}(
    x => Dict(lowercase(f) => v for (f,v) in d) for (x,d) in json.props)

  for f in hom(s)
    for i in json.parts[dom(s,f)]
      acs[ob_maps[dom(s,f)][i],f] = ob_maps[codom(s,f)][props[i][f]]
    end
  end

  for f in attr(s)
    for i in json.parts[dom(s,f)]
      if f ∈ keys(props[i])
        acs[ob_maps[dom(s,f)][i],f] = deserializers[codom(s,f)](props[i][f])
      end
    end
  end

  acs
end

end
