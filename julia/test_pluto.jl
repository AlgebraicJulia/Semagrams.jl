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

# ╔═╡ 7795aa9d-5285-4ae3-a10b-114707f3516c
using Catlab.Schemas

# ╔═╡ 1228203a-d5c0-496c-bb1c-9b0fa9bab877
using Catlab.DenseACSets, Catlab.SparseACSets

# ╔═╡ 6253111f-7ee2-4cd0-aa37-7f489da8d595
SchGraph = Schema([:E,:V],Dict(:Src=>DomCodom(:E,:V),:Tgt=>DomCodom(:E,:V)),Symbol[], Dict{Symbol,DomCodom}())

# ╔═╡ 0658ea58-8f11-4439-86c6-6e3ed6138821
SchPetri =
	Schema(
		[:S,:T,:I,:O],
		Dict(
			:IT => DomCodom(:I, :T),
			:IS => DomCodom(:I, :S),
			:OT => DomCodom(:O, :T),
			:OS => DomCodom(:O, :S)
		),
		Symbol[],
		Dict{Symbol,DomCodom}()
	)

# ╔═╡ acf2bff2-2126-4b17-98ae-7614bdc0e33d
@bind g Semagram("http://127.0.0.1:8080/apps/petri/target/scala-3.1.3/semagrams-petri-app-fastopt/main.js", SchPetri, "Petri")

# ╔═╡ c280e9c1-104e-4fe8-a55a-5df45c6c2f69
@acset_type Petri(SchPetri, index=[:src,:tgt])

# ╔═╡ 68ceeeea-26a9-43cb-be5f-ab1801db02b2
make_dense(g, Petri)

# ╔═╡ Cell order:
# ╠═b7eab914-f77f-423f-b369-739dd060ec4a
# ╠═a448f946-c3a2-4895-a168-1af2d73dc576
# ╠═0e1eb63b-06d2-4247-b3fc-f547caa2f681
# ╠═7795aa9d-5285-4ae3-a10b-114707f3516c
# ╟─6253111f-7ee2-4cd0-aa37-7f489da8d595
# ╠═0658ea58-8f11-4439-86c6-6e3ed6138821
# ╠═acf2bff2-2126-4b17-98ae-7614bdc0e33d
# ╠═1228203a-d5c0-496c-bb1c-9b0fa9bab877
# ╠═c280e9c1-104e-4fe8-a55a-5df45c6c2f69
# ╠═68ceeeea-26a9-43cb-be5f-ab1801db02b2
