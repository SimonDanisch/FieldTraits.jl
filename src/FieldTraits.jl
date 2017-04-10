__precompile__(true)
module FieldTraits

using Compat
import Base: @propagate_inbounds, @pure, tail, haskey, getindex, setindex!, get
import Base: is_linenumber, convert, (==)

@compat abstract type Composable end

include("composedtype.jl")


@compat abstract type  ReactiveComposable <: Composable end

include("reactive_composable.jl")

end # module
