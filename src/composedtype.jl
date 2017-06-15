const Field = Composable
const ComposableLike = Union{Composable, Tuple{Vararg{Pair}}}


"""
    UsageError(binding, value)
    UsageError(message::String, value)

Exception type for incorrect usage of some type or function `binding`.
Defaults to throwing the docstring as a message, but you can supply a
custom `message`. Any arguments can be supplied as `value`.
"""
immutable UsageError <: Exception
    message::String
    value
    function UsageError(message::String, value)
        new(message, value)
    end
end
function UsageError(binding, value)
    message = stringmime("text/plain", Docs.doc(binding))
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
    Fields(typ)

Gets a tuple of all field types of a composable
"""
Fields(x::Type{<:Composable}) = ()
Fields(x::Composable) = Fields(typeof(x))

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
    @needs composable: Field1, Field2, ...
    @needs composable: Field1, name2 = Field2, ...

Asserts that a `composable` has the listed fields. Optionally assign a
variable binding to chosen fields; this variable has scope within the
containing block (e.g., function definition).
"""
macro needs(expr::Expr)
    comp_fields = @match expr begin
        (composable_:f1_)  => (composable, Any[f1])
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
An partially initialized composed type for type Parent
"""
immutable Partial{Parent, T}
    val::T
end
Partial{P, T}(::Type{P}, x::T) = Partial{P, T}(x)
Partial{P}(::Type{P}) = Partial(P, ComposedDict())

"""
    default(FieldType)
    default(parent, FieldType)

Construct a default for `FieldType`.  Can be specialized for different
`parent` objects.  If you do not specialize it for type `FieldType`, this
calls `FieldType()`. Values optionally holds a partially initialised Partial object, e.g.
when called like Partial(A => x, B => c), A and B will be supplied via values.
This way we can construct better defaults!
"""
function default{P <: ComposableLike, F <: Field}(
        ::Type{F}, ::Type{P},
    )
    default(F, Partial(P))
end

default{F <: Field}(::Type{F}, ::Partial) = default(F)
default{F <: Field}(::Type{F}) = F()

"""
    convert(ComposableType, FieldType, x)

Converts field `FieldType` in `x` to the type of field `FieldType` in
`ComposableType`. `x` may be supplied as another composable (thus
extracting `x[FieldType]`), or as a tuple of `FieldType => value` pairs.
"""
function convertfor{F <: Field, P}(::Type{F}, x::Partial{P}, value)
    convert(cfieldtype(P, F), value)
end


"""
    fieldindex(ComposableType, FieldType)

Calculates the index into a struct type from a Composable type. The
return is a tuple of `Val{i}` instances, where `i` is the index of
`FieldType` in `ComposableType`. If `FieldType` is not a field of
`ComposableType`, then `i` is 0.

# Example

```jldocstring
@composed type Composed
    Scale
    Position
end
fieldindex(Composed, Position) == (Val{2}(),)
```
"""
fieldindex{T <: Composable, F <: Field}(::Type{T}, ::Type{F}) = Val{0}()
fieldindex{T <: Composable, F <: Field}(::T, ::Type{F}) = fieldindex(T, F)


Base.@inline haskey{T <: Composable, N}(::Type{T}, field::Val{N}) = N > 0
Base.@pure haskey{T <: Composable, F <: Field}(c::Type{T}, field::Type{F}) = haskey(T, fieldindex(T, F))
Base.@pure haskey{T <: Composable, F <: Field}(c::T, field::Type{F}) = haskey(T, fieldindex(T, F))

# Would like to extend Base.fieldtype, but that is an Builtin function which can't
# be extended.
"""
    cfieldtype(ComposableType, FieldType)

Return the type of `FieldType` in a composed type `ComposedType`. The default
is `Any`. This is automatically specialized by `@composed`.
"""
cfieldtype{T <: Composable, F <: Field}(ct::T, ::Type{F}) = cfieldtype(T, fieldindex(T, F), F)
cfieldtype{T <: Composable, F <: Field}(ct::Type{T}, ::Type{F}) = cfieldtype(T, fieldindex(T, F), F)
function cfieldtype{T <: Composable, N}(::Type{T}, ::Val{N}, field = N)
    if N === 0
        error("Can't get $field from type $T")
    else
        fieldtype(T, N)
    end
end
"""
    fieldconvert(ComposableType, FieldType, value)

Converts `value` to the type needed for field `FieldType` in type `ComposableType`.
"""
function fieldconvert{T <: Composable}(ct::T, field, value)
    convert(cfieldtype(T, field), value)
end


@propagate_inbounds function _setindex!{N}(ct::Composable, val, field::Val{N})
    setfield!(ct, N, fieldconvert(ct, field, val))
end

@propagate_inbounds function _setindex!{F <: Field}(ct::Composable, val, ::Type{F})
    idx = fieldindex(ct, F)
    if !isa(idx, Val{0})
        _setindex!(ct, val, idx)
    else
        throw(BoundsError(ct, F))
    end
end

@propagate_inbounds function setindex!{F <: Field}(ct::Composable, value, field::Type{F})
    _setindex!(ct, value, field)
end
@propagate_inbounds function getindex{N}(ct::Composable, field::Val{N})
    getfield(ct, N)
end
@propagate_inbounds function getindex{F <: Field}(ct::Composable, ::Type{F})
    idx = fieldindex(ct, F)
    if !isa(idx, Val{0})
        return getindex(ct, idx)
    else
        throw(BoundsError(ct, F))
    end
end

Base.length(x::Composable) = length(Fields(x))
Base.start(x::Composable) = (Fields(x), 1)
function Base.next(x::Composable, state)
    fields, idx = state
    field = fields[idx]
    (field => x[field], (fields, idx + 1))
end
Base.done(x::Composable, state) = state[2] > length(state[1])

"""
Default empty constructor.
Will initialize fields with `default(T, field)`
"""
function (t::Type{T}){T <: Composable}()
    fields = Fields(T)
    if isempty(fields) # we're at a leaf field without an empty constructor defined
        # TODO think of good error handling, that correctly advises the user
        error("No default for $t")
    end
    p = Partial(T)
    T(map(field-> default(field, p), fields)...)
end

# Constructor from partial composed type (tuple of pairs)
"""
Partially initialization constructor.
You can supply values for fields as pairs, and the rest will be filled in by
`default(T, field, pairs)`.
"""
function (::Type{T}){T <: Composable}(c::ComposableLike)
    partial1 = Partial(T, c)
    conv_fields = ComposedDict(Dict{DataType, Any}(map(c) do fieldval
        Field, val = fieldval
        if haskey(T, Field)
            Field => convertfor(Field, partial1, val) # converts might be expensive or a noop, so lets do it first
        else
            fieldval
        end
    end))
    partialconv = Partial(T, conv_fields)
    fields = map(Fields(T)) do Field
        x = if haskey(conv_fields, Field)
            get(conv_fields, Field)
        else
            default(Field, partialconv)
        end
        if !haskey(partialconv, Field)
            partialconv[Field] = x
        end
        x
    end
    T(fields...)
end

"""
    @field FieldType

Creates a type `FieldType` as a component of a Composable type.
"""
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
        Base.@__doc__ immutable $name <: $supertyp end
        $default_expr
    end
end





function add_fields!(composedfields, result, fields, T_args, ftype_funcs, idx = 1)
    for field in composedfields
        sym, Field, T = field, field, field
        if is_linenumber(field)
            #push!(result, field)
            continue
        # untyped field which will get a parameter
        elseif isa(field, Symbol) || isa(field, GlobalRef) ||
                (isa(field, Expr) && field.head == :(.))
            Field = eval(current_module(), field)
            sym = Symbol(field2string(Field))
            T = sym
            push!(T_args, Field => T)
        elseif isa(field, Expr) # typed field with no parameter
            if field.head == :(::)
                sym, T = field.args
                Field = eval(current_module(), sym)
                push!(ftype_funcs, Field => T)
            elseif field.head == :(.) || isa(field, GlobalRef) # untyped field which will get a parameter
                push!(T_args, field => field)
                Field = eval(current_module(), field)
            else
                match = @match field begin
                    (<: Composable_) => begin
                        T = eval(current_module(), Composable)
                        idx = add_fields!(Fields(T), result, fields, T_args, idx)
                        continue
                    end
                end
                match == nothing && error("Must be `field`, `field::T`, or `<: T`. Found: $field")
            end
        elseif isa(field, DataType) || (isdefined(Base, :UnionAll) && isa(field, UnionAll))
            sym = Symbol(field2string(field))
            T = sym; Field = field
            push!(T_args, field => T)
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

"""
Usage:
```Julia
@composed type Name [<: OptionalSuperType]
    <: InheritFieldsFromThisType
    FieldTrait1
    FieldTrait2::OptionalStrictType
end
```
"""
macro composed(expr::Expr)
    composed_type(expr)
end


function composed_type(expr::Expr, additionalfields = [], supertyp = Composable)
    name_supertyp_fields, mutable = @match expr begin
        type name_ <: supertyp_
            fields__
        end => (name, supertyp, fields), true
        type name_
            fields__
        end => (name, supertyp, fields), true

        immutable name_ <: supertyp_
            fields__
        end => (name, supertyp, fields), false
        immutable name_
            fields__
        end => (name, supertyp, fields), false
    end
    if name_supertyp_fields == nothing
        throw(UsageError(composed, expr))
    end
    name, supertyp, fields = name_supertyp_fields
    composedfields = [additionalfields; fields...]
    typedfields = []; fields = []; FT_args = []; ftype_funcs = []
    add_fields!(composedfields, typedfields, fields, FT_args, ftype_funcs)
    idxfuncs = Expr(:block)
    for (i, Field) in enumerate(fields)
        push!(idxfuncs.args,
            :(FieldTraits.fieldindex(::Type{<: $name}, ::Type{$Field}) = Val{$i}())
        )
    end
    T_args = last.(FT_args)
    fieldfuncs = quote
        FieldTraits.Fields(::Type{$name}) = ($(fields...),)
    end
    typ = Expr(
        :type, mutable,
        :($(name){$(T_args...)} <: $supertyp),
        Expr(:block, typedfields...)
    )
    expr = quote
        Base.@__doc__ $typ
        $(fieldfuncs)
        $(idxfuncs)
    end
    esc(expr)
end


# A dictionary wrapper supporting the ComposedApi
immutable ComposedDict{V} <: Composable
    data::Dict{DataType, V}
end
ComposedDict() = ComposedDict(Dict{DataType, Any}())

Base.start(x::ComposedDict) = start(x.data)
Base.next(x::ComposedDict, state) = next(x.data, state)
Base.done(x::ComposedDict, state) = done(x.data, state)
Base.length(x::ComposedDict) = length(x.data)

function getindex{F <: Field}(cd::ComposedDict, ::Type{F})
    # TODO search!
    getindex(cd.data, F)
end

function setindex!{F <: Field}(cd::ComposedDict, val, ::Type{F})
    # TODO search!
    setindex!(cd.data,val, F)
end
function setindex!{F <: Field}(cd::Partial, val, ::Type{F})
    # TODO search!
    setindex!(cd.val, val, F)
end
function get!{F <: Field}(cd::ComposedDict, ::Type{F}, default)
    get!(cd.data, F, default)
end

Fields(::Type{ComposedDict}) = ()
Fields(::ComposedDict) = ()

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

haskey{F <: Field}(cd::ComposedDict, ::Type{F}) = haskey(cd.data, F)

function haskey{F <: Field}(x::Partial, ::Type{F})
    haskey(x.val, F)
end
haskey{F <: Field}(x::Tuple{}, ::Type{F}) = false

function haskey{N, F <: Field}(x::Tuple{Vararg{Pair, N}}, ::Type{F})
    a, b = first(x)
    a <: F && return true
    haskey(Base.tail(x), F)
end

##################################################################
# get
#

# implements fallback to default
# implements fallback to default
function get{F <: Field}(p::Partial, ::Type{F})
    get(()-> default(F, p), p.val, F)
end
function get!{F <: Field}(cd::Partial, ::Type{F}, default...)
    x = get(cd, F, default...)
    cd.val[F] = x
    x
end

function get{T <: ComposableLike, F <: Field}(x::T, ::Type{F})
    get(()-> default(F, Partial(T, x)), x, F)
end
# implements to return a default value
function get{F <: Field}(x::ComposableLike, ::Type{F}, default)
    get(()-> default, x, F)
end


get{F <: Field}(Func, x::Tuple{}, ::Type{F}) = Func()
function get{N, F <: Field}(Func, x::Tuple{Vararg{Pair, N}}, ::Type{F})
    a, b = first(x)
    a <: F && return b
    get(Func, Base.tail(x), F)
end

function get{F <: Field}(Func, x::Composable, ::Type{F})
    haskey(x, F) && return x[F]
    Func()
end

function (==)(x::Composable, y::Composable)
    f1, f2 = Fields(x), Fields(y)
    f1 != f2 && return false
    for f in f1
        x[f] == y[f] || return false
    end
    true
end
