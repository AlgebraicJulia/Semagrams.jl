### A Pluto.jl notebook ###
# v0.19.9

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

# ╔═╡ b7eab914-f77f-423f-b369-739dd060ec4a
begin
	import Pkg
	Pkg.activate(".")
end

# ╔═╡ a448f946-c3a2-4895-a168-1af2d73dc576
using Revise

# ╔═╡ 0e1eb63b-06d2-4247-b3fc-f547caa2f681
using Semagrams

# ╔═╡ 74cef662-a15f-4c17-b652-8fdc88f1db47
using AlgebraicPetri

# ╔═╡ 1f148be2-ef69-4ae3-a031-d11a6baf85d5
using DifferentialEquations

# ╔═╡ 69c15116-f88b-48f9-86e5-0358f1bf266a
using Plots

# ╔═╡ acf2bff2-2126-4b17-98ae-7614bdc0e33d
@bind petri Semagram{LabelledReactionNet{Float64, Float64}}(
	"http://127.0.0.1:8080/apps/petri/target/scala-3.1.3/semagrams-petri-app-fastopt/main.js",
	"Petri",
	Dict{Symbol,Function}(
		:Name => s -> Symbol(s),
		:Rate => s -> s,
		:Concentration => s -> s,
	),
)

# ╔═╡ 6d398559-c235-44b8-836a-6d22c9c15862
begin
	if ns(petri) > 0
		u0 = concentrations(petri)
		p = Vector{Float64}(rates(petri))
		prob = ODEProblem(vectorfield(petri), u0, (0.0, 5.0), p)
		sol = solve(prob, Tsit5())
		plot(sol)
	end
end

# ╔═╡ Cell order:
# ╠═b7eab914-f77f-423f-b369-739dd060ec4a
# ╠═a448f946-c3a2-4895-a168-1af2d73dc576
# ╠═0e1eb63b-06d2-4247-b3fc-f547caa2f681
# ╠═74cef662-a15f-4c17-b652-8fdc88f1db47
# ╠═1f148be2-ef69-4ae3-a031-d11a6baf85d5
# ╠═69c15116-f88b-48f9-86e5-0358f1bf266a
# ╠═acf2bff2-2126-4b17-98ae-7614bdc0e33d
# ╠═6d398559-c235-44b8-836a-6d22c9c15862
