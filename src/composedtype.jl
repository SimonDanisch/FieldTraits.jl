const Field = Composable

"""
Gets a tuple of all field types of a composable
"""
function Fields end

"""
default([Parent], field) constructs a default for `field`.
Can be overloaded for different parents and falls back to empty field constructor
"""
function default{Parent, F <: Field}(x::Parent, ::Type{F})
    default(F)
end
function default{F <: Field}(::Type{F})
    F()
end

# default for overloading conversion!
# Parent can be a composable or any other guiding object for convert
function convert{F <: Field}(::Type{F}, parent, val)
    val
end

"""
Calculates the index into a struct type from a Composable type.
E.g.
```
    @composed type Test
        Scale
        Position
    end
    fieldindex(Test, Position) == 2
```
"""
fieldindex{T <: Composable, F <: Field}(::Type{T}, ::Type{F}) = (Val{0}(),)
fieldindex{T <: Composable, F <: Field}(::T, ::Type{F}) = fieldindex(T, F)


function haskey{T <: Composable}(c::T, field::Tuple)
    haskey(c, field) > 0 && haskey(c, tail(field))
end
haskey{T <: Composable, N}(c::T, field::Val{N}) = N > 0
haskey{T <: Composable, N}(c::T, field::Tuple{Val{N}}) = N > 0

function haskey{T <: Composable, F <: Field}(c::T, field::Type{F})
    haskey(c, fieldindex(T, F))
end

"""
Returns the type of a field in a composed type.
Would like to extend Base.fieldtype, but that is an Builtin function which can't
be extended
"""
function cfieldtype(ct::Composable, field)
    typeof(getindex(ct, field))
end

"""
Converts a value to the field type of field in a composed type.
"""
function fieldconvert(ct::Composable, field, value)
    convert(cfieldtype(ct, field), value)
end
@propagate_inbounds function _setindex!{N}(ct::Composable, val, field::Val{N})
    setfield!(ct, N, fieldconvert(ct, field, val))
end
@propagate_inbounds function _setindex!{N}(ct::Composable, val, field::Tuple{Val{N}})
    setfield!(ct, N, fieldconvert(ct, field, val))
end
@propagate_inbounds function _setindex!(ct::Composable, val, field::Tuple)
    prim = ct[Base.front(field)]
    _setindex!(prim, val, last(field))
end
@propagate_inbounds function _setindex!{F <: Field}(ct::Composable, val, ::Type{F})
    _setindex!(ct, val, fieldindex(ct, F))
end
@propagate_inbounds function setindex!{F <: Field}(ct::Composable, value, field::Type{F})
    _setindex!(ct, value, field)
end
@propagate_inbounds function getindex{N}(ct::Composable, field::Val{N})
    getfield(ct, N)
end
@propagate_inbounds function getindex{N}(ct::Composable, field::Tuple{Val{N}})
    getfield(ct, N)
end
@propagate_inbounds function getindex(ct::Composable, field::Tuple)
    getindex(getindex(ct, first(field)), tail(field))
end
@propagate_inbounds function getindex{F <: Field}(ct::Composable, ::Type{F})
    getindex(ct, fieldindex(ct, F))
end

# Default Constructor, empty constructor
function (::Type{T}){T <: Composable}()
    fields = Fields(T)
    if isempty(fields) # we're at a leaf field without an empty constructor defined
        # TODO think of good error handling, that correctly advises the user
        error("No default for $T")
    end
    T(map(field-> default(T, field), fields)...)
end

# Constructor from partial composed type (tuple of pairs)
(::Type{T}){T <: Composable, N}(c::Vararg{Pair, N}) = T(c)
function (::Type{T}){T <: Composable, N}(c::Tuple{Vararg{Pair, N}})
    c_converted = map(c) do fieldval
        field, val = fieldval
        field => convert(field, T, val) # converts might be expensive or a noop, so lets do it first
    end
    fields = map(Fields(T)) do field
        get(c_converted, field)
    end
    T(fields...)
end

# Constructor from another Composable type
function (::Type{T}){T <: Composable}(c::Composable)
    fields = map(Fields(T)) do Field
        convert(field, T, get(c, field))
    end
    T(fields...)
end

# Decouple implementation of field from declaration by using a macro
macro field(name::Symbol)
    esc(quote
        immutable $name <: FieldTraits.Field end
        FieldTraits.Fields(::Type{$name}) = ()
    end)
end

function default_construction(main, defaults)
    usage = """Please supply defaults in block syntax. E.g:
    @field Name begin
        Name = :MyName
    end"""
    if defaults.head != :block
        error(usage)
    end
    default_constructors = Expr(:block)
    for elem in defaults.args
        if Base.is_linenumber(elem)
            push!(default_constructors.args, elem)
            continue
        elseif !isa(elem, Expr) && elem.head != :(=)
            error(usage)
        end
        field, default_body = elem.args
        if !isa(field, Symbol)
            error(usage)
        end
        constr_expr = quote
            FieldTraits.default{T <: $field}(::Type{T}) = $default_body
        end
        push!(default_constructors.args, constr_expr)
    end
    default_constructors
end
macro field(name::Symbol, defaults)
    constructors = default_construction(name, defaults)
    esc(quote
        immutable $name <: FieldTraits.Field end
        FieldTraits.Fields(::Type{$name}) = ()
        $constructors
    end)
end

"""
Recursively adds fieldindex methods for composed types.
E.g.:
```
@composed Transform
    Scale
    Rotation
    Position
end
@composed Test
    Transform
end
Will result in Test having fieldindex methods also for Scale Rotation and position
```
"""
function add_fieldindex(Field, block, idx, name)
    push!(block.args,
        :(FieldTraits.fieldindex{T <: $name}(::Type{T}, ::Type{$Field}) = $idx)
    )
    if Field <: Composable
        for (i, T) in enumerate(Fields(Field))
            newidx = Expr(:tuple, idx.args..., :(Val{$i}()))
            add_fieldindex(T, block, newidx, name)
        end
    end
end


function composed_type(expr, additionalfields = [], supertype = Composable)
    @assert expr.head == :type
    name = expr.args[2]
    idxfuncs = Expr(:block)
    parameters = []
    fields = []
    T_args = []
    idx = 1
    composedfields = [additionalfields; expr.args[3].args]
    typedfields = map(composedfields) do field
        sym, Field, T = if isa(field, Symbol)
            push!(T_args, field)
            field, eval(current_module(), field), field
        elseif isa(field, Expr) && field.head == :(::)
            sym, T = field.args
            sym, eval(current_module(), sym), T
        elseif isa(field, DataType)
            sym = typename(field).name
            push!(T_args, sym)
            sym, field, sym
        elseif is_linenumber(field)
            return field
        else
            error("Unsupported expr: $field")
        end
        fname = Symbol(lowercase(string(sym)))
        push!(fields, Field)
        # Recursively add fieldinex methods
        add_fieldindex(Field, idxfuncs, :((Val{$idx}(),)), name)

        idx += 1

        :($fname::$T)
    end
    fieldfuncs = quote
        FieldTraits.Fields(::Type{$name}) = ($(fields...),)
        FieldTraits.Fields(::$name) = ($(fields...),)
    end
    tname, supertype = if isa(name, Symbol)
        name, supertype
    elseif isa(name, Expr) && name.head == :(<:)
        name.args
    else
        error("Unsupported expr $name")
    end
    expr = quote
        type $(tname){$(T_args...)} <: $supertype
            $(typedfields...)
        end
        $(esc(fieldfuncs))
        $(esc(idxfuncs))
    end
    expr

end
"""
"""
macro composed(expr)
    composed_type(expr)
end

# A dictionary wrapper supporting the ComposedApi
immutable ComposedDict{V} <: Composable
    data::Dict{Symbol, V}
end

haskey{F <: Field}(cd::ComposedDict, ::Type{F}) = haskey(cd.data, typename(F).name)

function getindex{F <: Field}(cd::ComposedDict, ::Type{F})
    # TODO search!
    getindex(cd.data, typename(F).name)
end

function setindex!{F <: Field}(cd::ComposedDict, val, ::Type{F})
    # TODO search!
    setindex!(cd.data,val, typename(F).name)
end
function get!{F <: Field}(cd::ComposedDict, ::Type{F}, default)
    get!(cd.data, typename(F).name, default)
end

Fields(::Type{ComposedDict}) = ()
Fields(::ComposedDict) = ()
(::Type{ComposedDict})() = ComposedDict(Dict{Symbol, Any}())

# helper to use tuples as partial Composed Traits
function Base.getindex{F <: Field}(x::Tuple{}, ::Type{F})
    error("Tuple doesn't have field $F")
end
# @pure be legal and helpful here?
@propagate_inbounds function Base.getindex{N, F <: Field}(x::Tuple{Vararg{Pair, N}}, ::Type{F})
    a, b = first(x)
    a <: F && return b
    getindex(Base.tail(x), F)
end

haskey{F <: Field}(x::Tuple{}, ::Type{F}) = false
function haskey{N, F <: Field}(x::Tuple{Vararg{Pair, N}}, ::Type{F})
    a, b = first(x)
    a <: F && return true
    haskey(Base.tail(x), F)
end

get{F <: Field}(Func, x::Tuple{}, ::Type{F}) = Func()
function get{N, F <: Field}(Func, x::Tuple{Vararg{Pair, N}}, ::Type{F})
    a, b = first(x)
    a <: F && return b
    get(Func, Base.tail(x), F)
end


_get{F <: Field}(tup, x::Tuple{}, ::Type{F}) = default(tup, F)
function _get{N, F <: Field}(tup, x::Tuple{Vararg{Pair, N}}, ::Type{F})
    a, b = first(x)
    a <: F && return b
    _get(tup, Base.tail(x), F)
end
@inline function get{N, F <: Field}(x::Tuple{Vararg{Pair, N}}, ::Type{F})
    _get(x, x, F)
end

function (==)(x::Composable, y::Composable)
    f1, f2 = Fields(x), Fields(y)
    f1 != f2 && return false
    for f in f1
        x[f] == y[f] || return false
    end
    true
end
