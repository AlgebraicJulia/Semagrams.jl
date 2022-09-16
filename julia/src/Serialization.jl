module Serialization
export read_acset

using Catlab.Schemas, Catlab.SparseACSets
import JSON3

function read_acset(name::String, s::Schema, json::JSON3.Object)
  acs = DynamicSparseACSet(name, s)
  for ob in s.obs
    acs.parts[ob] = Set(json.obs[ob])
  end
  for (hom,dc) in s.homs
    vs = Dict(map(Tuple, [json.homs[hom]...]))
    for i in acs.parts[dc.dom]
      acs[i,hom] = vs[i]
    end
  end
  acs
end

end
