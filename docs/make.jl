using Documenter
using Semagrams

makedocs(
    sitename = "Semagrams",
    format = Documenter.HTML(),
    modules = [Semagrams]
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
deploydocs(
  repo = "github.com/AlgebraicJulia/Semagrams.jl.git",
  devbranch = "main"
)
