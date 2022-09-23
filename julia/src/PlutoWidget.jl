module PlutoWidget

export Semagram

using HypertextLiteral
import AbstractPlutoDingetjes
import JSON3
using Catlab.CSetDataStructures

using ..Serialization

struct Semagram{T<:ACSet}
  script_url::String
  name::String
  deserializers::Dict{Symbol, Function}
end

function Base.show(io, m::MIME"text/html", s::Semagram)
  show(io, m, @htl("""
    <div>
      <script>
        const { main } = await import($(s.script_url))
        main(currentScript.parentElement)
      </script>
    </div>
    """))
end

AbstractPlutoDingetjes.Bonds.initial_value(s::Semagram) = "{}"
AbstractPlutoDingetjes.Bonds.transform_value(s::Semagram, v::Nothing) = v
AbstractPlutoDingetjes.Bonds.transform_value(s::Semagram{T}, v) where {T} = read_acset(s.name, T, JSON3.read(v), s.deserializers)

end
