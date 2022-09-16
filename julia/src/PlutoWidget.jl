module PlutoWidget

export Semagram

using HypertextLiteral
import AbstractPlutoDingetjes
import JSON3
using Catlab.Schemas

using ..Serialization

struct Semagram
  script_url::String
  schema::Schema
  name::String
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
AbstractPlutoDingetjes.Bonds.transform_value(s::Semagram, v) = read_acset(s.name, s.schema, JSON3.read(v))

end
