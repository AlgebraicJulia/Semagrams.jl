module Serialization
export read_acset

using Catlab.CSetDataStructures
import JSON3

function read_acset(name::String, ::Type{T}, json::JSON3.Object, deserializers::Dict{Symbol, Function}) where {T <: ACSet}
  acs = T()
  s = acset_schema(acs)
  ob_maps = Dict{Symbol, Dict{Int, Int}}()
  for ob in s.obs
    ob_maps[ob] = Dict{Int,Int}()
    for i in json.obs[ob]
      ob_maps[ob][i] = add_part!(acs, ob)
    end
  end

  for f in s.homs
    vs = Dict(map(Tuple, [json.homs[f]...]))
    for i in json.obs[s.doms[f]]
      acs[ob_maps[s.doms[f]][i],f] = ob_maps[s.codoms[f]][vs[i]]
    end
  end

  for f in s.attrs
    vs = Dict(map(Tuple, [json.attrs[f]...]))
    for i in json.obs[s.doms[f]]
      if i ∈ keys(vs)
        acs[ob_maps[s.doms[f]][i],f] = deserializers[s.codoms[f]](vs[i])
      end
    end
  end

  acs
end

end
