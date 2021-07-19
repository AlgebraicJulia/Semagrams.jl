"""
Our own little home-grown JSON conversion module (Muesli is a cereal...)

Types should overload `to_json` to support serialization
into json, and `from_json` to support deserialization out of json.

`to_json` and `from_json` do not actually convert into the string
format of json, they just convert into a subset of Julia types
(i.e. Dict, Vector, String, Int, Bool) that map cleanly into json.

WebIO handles the conversion between this and actual JSON.

We provide `generic_to_json` and `generic_from_json` that do
what one would expect on structs, mapping between a struct and a
dictionary of fields, converting the fields into/out of json as well.
"""
module Muesli

export to_json, generic_to_json, from_json, generic_from_json

"""
For a lot of types, (i.e. Int, String, Bool, etc.) the correct behavior
is for `to_json` to be the identity.
"""
to_json(x) = x

"""
For Dict{Int,X}, we want to convert to a vector of tuples
"""
to_json(x::Dict{Int,T}) where {T} = [(k,to_json(v)) for (k,v) in x]

"""
If x is a struct, this converts x into a dictionary whose keys are
the fieldnames of x, and whose values are the fields of x, converted.
"""
generic_to_json(x) = Dict(string(fn)=>to_json(getfield(x,fn)) for fn ∈ fieldnames(typeof(x)))

"""
`from_json` takes in a JSON value along with a type to convert into
and and attempts to convert to the type passed in.
"""
from_json(x,T) = convert(T,x) # fallback
from_json(x::String, ::Type{Symbol}) = Symbol(x)
from_json(x::String, ::Type{Union{Symbol, Nothing}}) = Symbol(x)
from_json(x::Symbol, ::Type{String}) = string(x)

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

function from_json(d::Vector{Tuple{Int,T}}, ::Type{Dict{Int,S}}) where {T,S}
  Dict(Int(i) => from_json(x,S) for (i,x) in d)
end

function from_json(s::String, ::Type{T}) where {T <: Real}
  parse(T,s)
end

function generic_from_json(d::Dict{String,<:Any},::Type{T}) where {T}
  augmented = Dict{Symbol,Any}()
  for (i,fn) in enumerate(fieldnames(T))
    sfn = string(fn)
    if sfn ∈ keys(d)
      augmented[fn] = from_json(d[sfn],fieldtypes(T)[i])
    else
      augmented[fn] = missing
    end
  end
  T([augmented[fn] for fn in fieldnames(T)]...)
end

end
