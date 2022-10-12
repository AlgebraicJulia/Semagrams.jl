module Serialization
export read_acset

using Catlab.CSetDataStructures
import JSON3

function read_acset(name::String, ::Type{T}, json::JSON3.Object, deserializers::Dict{Symbol, Function}) where {T <: ACSet}
  acs = T()
  s = acset_schema(acs)
  ob_maps = Dict{Symbol, Dict{Int, Int}}()
  for ob in ob(s)
    ob_maps[ob] = Dict{Int,Int}()
    for i in json.obs[ob]
      ob_maps[ob][i] = add_part!(acs, ob)
    end
  end

  for f in hom(s)
    vs = Dict(map(Tuple, [json.homs[f]...]))
    for i in json.obs[dom(s,f)]
      acs[ob_maps[dom(s,f)][i],f] = ob_maps[codom(s,f)][vs[i]]
    end
  end

  for f in attr(s)
    vs = Dict(map(Tuple, [json.attrs[f]...]))
    for i in json.obs[dom(s,f)]
      if i ∈ keys(vs)
        acs[ob_maps[dom(s,f)][i],f] = deserializers[codom(s,f)](vs[i])
      end
    end
  end

  acs
end

end
