__precompile__(true)
module FieldTraits

using Compat, MacroTools
using Compat.TypeUtils

using Base: @propagate_inbounds, @pure, tail, is_linenumber
import Base: (==), haskey, getindex, setindex!, get, get!, convert

@compat abstract type Composable end

export @field, @composed, @reactivecomposed, @needs, Fields

include("composedtype.jl")


@compat abstract type  ReactiveComposable <: Composable end

include("reactive_composable.jl")

end # module
