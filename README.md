[![Dev Docs](https://img.shields.io/badge/docs-dev-blue.svg)](https://algebraicjulia.github.io/Semagrams.jl/dev)

# Semagrams.jl

A graphical editor for graph-like structures.

## Trailer:

https://www.youtube.com/watch?v=b1xiY9sznEo

## Quickstart:

In a Jupyter notebook

``` julia
> using Semagrams, Semagrams.Examples

> p = Semagram{Petri}(PetriSema)

# Edit semagram (see docs)

> get_acset(p)
```
