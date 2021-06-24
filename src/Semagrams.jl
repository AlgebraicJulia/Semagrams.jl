module Semagrams

include("SVG.jl")
include("Boxes.jl")
include("Logo.jl")
include("Muesli.jl")
include("Schema.jl")
include("Data.jl")
include("UI.jl")
include("Examples.jl")

using Reexport

@reexport using .SVG
@reexport using .Boxes
@reexport using .Logo
@reexport using .Muesli
@reexport using .Schema
@reexport using .Data
@reexport using .UI
# Examples are *not* reexported by default,
# because they could clash with similar definitions elsewhere

end
