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

# ╔═╡ 3fd1794d-6efc-4f4e-a193-cbc5af671ed6
using Catlab.SparseACSets

# ╔═╡ acf2bff2-2126-4b17-98ae-7614bdc0e33d
@bind sg Semagram("http://127.0.0.1:8080/apps/graph/target/scala-3.1.3/semagrams-graph-app-fastopt/main.js")

# ╔═╡ 6253111f-7ee2-4cd0-aa37-7f489da8d595
SchGraph = Schema([:E,:V],Dict(:Src=>DomCodom(:E,:V),:Tgt=>DomCodom(:E,:V)),Symbol[], Dict{Symbol,DomCodom}())

# ╔═╡ ab4850f4-1516-4566-9df7-a7b3af2da20c
begin
    g = DynamicSparseACSet("Graph", SchGraph)
	for ob in [:E,:V]
		g.parts[ob] = Set(sg.obs[ob])
	end
	for hom in [:Src,:Tgt]
		vs = Dict(map(Tuple, [sg.homs[hom]...]))
		for e in g.parts[:E]
			g[e,hom] = vs[e]
		end
	end
	g
end

# ╔═╡ 7ba62e16-60c1-4789-8a4d-cfcf66a4044c


# ╔═╡ Cell order:
# ╠═b7eab914-f77f-423f-b369-739dd060ec4a
# ╠═a448f946-c3a2-4895-a168-1af2d73dc576
# ╠═0e1eb63b-06d2-4247-b3fc-f547caa2f681
# ╠═acf2bff2-2126-4b17-98ae-7614bdc0e33d
# ╠═3fd1794d-6efc-4f4e-a193-cbc5af671ed6
# ╟─6253111f-7ee2-4cd0-aa37-7f489da8d595
# ╠═ab4850f4-1516-4566-9df7-a7b3af2da20c
# ╠═7ba62e16-60c1-4789-8a4d-cfcf66a4044c
