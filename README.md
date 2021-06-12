[![Dev Docs](https://img.shields.io/badge/docs-dev-blue.svg)](https://algebraicjulia.github.io/Semagrams.jl/dev)

# Semagrams.jl

A graphical editor for graph-like structures.

## Quickstart:

In a Jupyter notebook

``` julia
> using Semagrams, Semagrams.Examples

> p = Semagram{Petri}(PetriSema)

# Edit semagram (see docs) AND SAVE

> get_acset(p)
```
