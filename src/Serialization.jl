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

  for (f,d,c) in homs(s)
    for i in json.parts[d]
      acs[ob_maps[d][i],f] = ob_maps[c][props[i][f]]
    end
  end

  for (f,d,c) in attrs(s)
    for i in json.parts[d]
      if f ∈ keys(props[i])
        acs[ob_maps[d][i],f] = deserializers[c](props[i][f])
      end
    end
  end

  acs
end

end
