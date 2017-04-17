const Field = Composable

"""
Throws an error for the usage of some type/function.
Defaults to throwing the docstring as a message
"""
immutable UsageError <: Exception
    message::String
    value
end
function UsageError(c, value)
    message = stringmime("text/plain", Docs.doc(c))
    UsageError(message, value)
end

function Base.showerror(io::IO, e::UsageError)
    println(io, "Usage Error:")
    for line in split(e.message, r"\r?\n")
        println(io, "   ", line)
    end
    print(io, "Supplied value: ", e.value)
end

"""
Gets a tuple of all field types of a composable
"""
function Fields end

function field2string(field)
    tname = typename(field)
    mod = tname.module
    fname = tname.name
    mname = module_name(mod)
    # this is kinda terrible.. But we need nice names, and uniqueness of keyes.
    mname_str = mod == Main || module_parent(mod) == Main ? "" : string(mname, "_")
    string(mname_str, fname)
end
function field2symbol(field)
    name_str = field2string(field)
    Symbol(lowercase(name_str))
end

"""
Can be used to assert that a composable has certain fields.
Usage: `@needs composable: Field1, Field2, ...`
"""
macro needs(expr::Expr)
    comp_fields = @match expr begin
        (composable_:f1_, ftail__)  => (composable, Any[f1, ftail...,])
        (composable_:var1_ = field1_)  => (composable, Any[:($var1 = $field1),])
        (composable_:var1_ = field1_, ftail__)  => (composable, Any[:($var1 = $field1), ftail...,])
    end
    if comp_fields == nothing
        throw(UsageError("Usage: `@needs composable: Field1, name = Field2, ...`", expr))
    end
    composable, fields = comp_fields
    checks = Expr(:block)
    checks.args = map(fields) do field
        field_expr = @match field begin
            (var_ = field_) => begin
                quote
                    haskey($composable, $field) || error("$($composable) doesn't contain field $($field)")
                    $var = $composable[$field]
                end
            end
            field_ => begin
                :(haskey($((composable)), $((field))) || error("$($composable) doesn't contain field $($field)"))
            end
        end
        if field_expr == nothing
            throw(UsageError("Usage: `@needs composable: Field1, name = Field2, ...`", expr))
        end
        field_expr
    end
    esc(checks)
end

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

macro field(expr)
    usage = """
        "Must be @field Sym [<: OptionalSuperType] = default_body.
        Found: $expr
    """
    name_supertyp_default = @match expr begin
        (name_ <: supertyp_ = default_) => name, supertyp, default
        (name_ <: supertyp_) => name, supertyp, nothing
        (name_ = default_) => name, FieldTraits.Field, default
        (name_) => name, FieldTraits.Field, nothing
    end
    if name_supertyp_default == nothing
        error(usage)
    end
    name, supertyp, default = map(esc, name_supertyp_default)
    default_expr = if default != nothing
        :(FieldTraits.default{T <: $name}(::Type{T}) = $default)
    else
        :()
    end
    quote
        immutable $name <: $supertyp end
        FieldTraits.Fields(::Type{$name}) = ()
        $default_expr
    end
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
function add_fieldindex!(Field, block, idx, name)
    push!(block.args,
        :(FieldTraits.fieldindex{T <: $name}(::Type{T}, ::Type{$Field}) = $idx)
    )
    if Field <: Composable
        for (i, T) in enumerate(Fields(Field))
            newidx = Expr(:tuple, idx.args..., :(Val{$i}()))
            add_fieldindex!(T, block, newidx, name)
        end
    end
end

function add_fields!(composedfields, result, fields, T_args, idx = 1)
    for field in composedfields
        sym, Field, T = field, field, field
        if is_linenumber(field)
            push!(result, field)
            continue
        # untyped field which will get a parameter
        elseif isa(field, Symbol) || isa(field, GlobalRef) ||
                (isa(field, Expr) && field.head == :(.))
            Field = eval(current_module(), field)
            sym = Symbol(field2string(Field))
            T = sym
            push!(T_args, sym)
        elseif isa(field, Expr) # typed field with no parameter
            if field.head == :(::)
                sym, T = field.args
                Field = eval(current_module(), sym)
            elseif field.head == :(.) || isa(field, GlobalRef) # untyped field which will get a parameter
                push!(T_args, field)
                Field = eval(current_module(), field)
            elseif field.head == :(<:) # inherit fields
                if length(field.args) != 1
                    error("inheritance expression must be of form `<: Composable`")
                end
                tsym = field.args[1]
                T = eval(current_module(), tsym)
                idx = add_fields!(Fields(T), result, fields, T_args, idx)
                continue
            else
                error("Must be field::T, or <: T. Found: $field")
            end
        elseif isa(field, DataType) || (isdefined(Base, :UnionAll) && isa(field, UnionAll))
            sym = Symbol(field2string(field))
            push!(T_args, sym)
            T = sym
        else
            error("Unsupported expr: $field $(typeof(field))")
        end
        fname = field2symbol(Field)
        push!(fields, Field)
        push!(result, :($fname::$T))
        idx += 1
    end
    idx
end

function composed_type(expr::Expr, additionalfields = [], supertyp = Composable)
    name_supertyp_fields = @match expr begin
        type name_ <: supertyp_
            fields__
        end => (name, supertyp, fields)
        type name_
            fields__
        end => (name, supertyp, fields)
    end
    if name_supertyp_fields == nothing
        throw(UsageError("""
        Usage:
        ```Julia
        @composed type Name [<: OptionalSuperType]
            FieldTrait1
            FieldTrait2::OptionalStrictType
        end
        ```
        """, expr))
    end
    name, supertyp, fields = name_supertyp_fields
    composedfields = [additionalfields; fields...]
    typedfields = []; fields = []; T_args = []
    add_fields!(composedfields, typedfields, fields, T_args)
    idxfuncs = Expr(:block)
    for (i, Field) in enumerate(fields)
        add_fieldindex!(Field, idxfuncs, :((Val{$i}(),)), name)
    end
    fieldfuncs = quote
        FieldTraits.Fields(::Type{$name}) = ($(fields...),)
        FieldTraits.Fields(::$name) = ($(fields...),)
    end
    quote
        type $(name){$(T_args...)} <: $supertyp
            $(typedfields...)
        end
        $(esc(fieldfuncs))
        $(esc(idxfuncs))
    end
end

"""
Usage:
```Julia
@composed type Name [<: OptionalSuperType]
    FieldTrait1
    FieldTrait2::OptionalStrictType
end
```
"""
macro composed(expr::Expr)
    composed_type(expr)
end

# A dictionary wrapper supporting the ComposedApi
immutable ComposedDict{V} <: Composable
    data::Dict{DataType, V}
end

haskey{F <: Field}(cd::ComposedDict, ::Type{F}) = haskey(cd.data, F)

function getindex{F <: Field}(cd::ComposedDict, ::Type{F})
    # TODO search!
    getindex(cd.data, F)
end

function setindex!{F <: Field}(cd::ComposedDict, val, ::Type{F})
    # TODO search!
    setindex!(cd.data,val, F)
end
function get!{F <: Field}(cd::ComposedDict, ::Type{F}, default)
    get!(cd.data, F, default)
end

Fields(::Type{ComposedDict}) = ()
Fields(::ComposedDict) = ()
(::Type{ComposedDict})() = ComposedDict(Dict{DataType, Any}())

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
