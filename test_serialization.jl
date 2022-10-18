### A Pluto.jl notebook ###
# v0.19.13

using Markdown
using InteractiveUtils

# This Pluto notebook uses @bind for interactivity. When running this notebook outside of Pluto, the following 'mock version' of @bind gives bound variables a default value (instead of an error).
macro bind(def, element)
    quote
        local iv = try Base.loaded_modules[Base.PkgId(Base.UUID("6e696c72-6542-2067-7265-42206c756150"), "AbstractPlutoDingetjes")].Bonds.initial_value catch; b -> missing; end
        local el = $(esc(element))
        global $(esc(def)) = Core.applicable(Base.get, el) ? Base.get(el) : iv(el)
        el
    end
end

# ╔═╡ 436693c2-4ddf-11ed-2609-7f443f0585dd
begin
	import Pkg
	Pkg.activate(".")
end

# ╔═╡ 62e5b935-5fb4-4eae-bcda-f30eee9acd69
using Revise

# ╔═╡ a0c2c089-6e9e-4118-96d5-f3c0e19b6130
using Semagrams

# ╔═╡ 50f67cb7-654e-43ab-b895-3db4ff139b3f
using Catlab.Graphs

# ╔═╡ ac4bdb2d-ad51-4a1e-b190-bef3e3d22b7c
deserializers = Dict{Symbol,Function}(
		:Name => s -> Symbol(s),
		:Rate => s -> s,
		:Concentration => s -> s,
		:TransitionTypeValue => s -> 
			if length(s) == 0
				:none
			else
				Symbol(s[1])
			end,
		:Stratification => s -> Symbol[Symbol(str) for str in s]
	)

# ╔═╡ 7bbacdcf-3fb6-4aa6-a1b6-8a2164ac1544
@bind pj Semagram{Graph}("http://localhost:8080/out/apps/petri/fullLinkJS.dest/main.js", "Petri", deserializers)

# ╔═╡ 8cb698c7-2c67-4fd9-ba2c-137b8d47e4c7
pj

# ╔═╡ d38e9123-1842-4aab-8a97-3fce1bac9c98
import JSON3

# ╔═╡ 76a67987-09ce-4cf7-a440-0a04b64b658e
json = JSON3.read(pj)

# ╔═╡ 7b6e5a3f-d6e3-45b8-9765-be4ba27c6967
Base.lowercase(s::Symbol) = Symbol(lowercase(string(s)))

# ╔═╡ 38a99552-ba4f-4210-bd9f-ea6dda1355c3
props = Dict{Int, Dict{Symbol, Any}}(x => Dict(lowercase(f) => v for (f,v) in d) for (x,d) in json.props)

# ╔═╡ Cell order:
# ╠═436693c2-4ddf-11ed-2609-7f443f0585dd
# ╠═62e5b935-5fb4-4eae-bcda-f30eee9acd69
# ╠═a0c2c089-6e9e-4118-96d5-f3c0e19b6130
# ╠═50f67cb7-654e-43ab-b895-3db4ff139b3f
# ╠═ac4bdb2d-ad51-4a1e-b190-bef3e3d22b7c
# ╠═7bbacdcf-3fb6-4aa6-a1b6-8a2164ac1544
# ╠═8cb698c7-2c67-4fd9-ba2c-137b8d47e4c7
# ╠═d38e9123-1842-4aab-8a97-3fce1bac9c98
# ╠═76a67987-09ce-4cf7-a440-0a04b64b658e
# ╠═7b6e5a3f-d6e3-45b8-9765-be4ba27c6967
# ╠═38a99552-ba4f-4210-bd9f-ea6dda1355c3
