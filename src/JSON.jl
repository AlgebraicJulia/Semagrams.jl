module JSON

export to_json, generic_to_json, from_json, generic_from_json

to_json(x) = x

generic_to_json(x) = Dict(string(fn)=>to_json(getfield(x,fn)) for fn ∈ fieldnames(typeof(x)))

from_json(x,T) = convert(T,x) # fallback
from_json(x::String,::Type{Symbol}) = Symbol(x)

function from_json(x::Vector{Any},::Type{Vector{T}}) where {T}
  map(y -> from_json(y,T), x)
end

function from_json(x::Vector{Any},::Type{Tuple{A,B}}) where {A,B}
  (from_json(x[1],A),from_json(x[2],B))
end

function from_json(x::Vector{Any},::Type{Pair{A,B}}) where {A,B}
  from_json(x[1],A) => from_json(x[2],B)
end

function from_json(x::Vector{Any},::Type{Dict{Int,T}}) where {T}
  Dict{Int,T}(from_json(x,Vector{Pair{Int,T}})...)
end

function from_json(d::Dict{String,Any}, ::Type{Dict{Symbol,T}}) where {T}
  Dict(Symbol(k) => from_json(v,T) for (k,v) in d)
end

function generic_from_json(d::Dict{String,Any},::Type{T}) where {T}
  augmented = Dict{Symbol,Any}()
  for (i,fn) in enumerate(fieldnames(T))
    sfn = string(fn)
    if sfn ∈ keys(d)
      augmented[fn] = from_json(d[string(fn)],fieldtypes(T)[i])
    else
      augmented[fn] = missing
    end
  end
  T([augmented[fn] for fn in fieldnames(T)]...)
end

end
