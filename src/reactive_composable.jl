@field Links = FieldTraits.ComposedDict()

"""
Composable, that allows one to link fields and register callbacks to field changes
"""
macro reactivecomposed(expr)
    composed_type(expr, [Links], ReactiveComposable)
end

immutable _NoUpdate end
const NoUpdate = _NoUpdate()

@propagate_inbounds function setindex!{F <: Field}(ct::ReactiveComposable, value, field::Type{F}, ::_NoUpdate)
    _setindex!(ct, value, field)
end
@propagate_inbounds function setindex!{F <: Field}(ct::ReactiveComposable, value, field::Type{F})
    _setindex!(ct, value, field)
    links = ct[Links]
    if haskey(links, field)
        link = links[field]
        for (func, fields, args) in link
            func(map(f-> ct[f], fields)..., args...)
        end
    end
    value
end



"""
Links a field with another in to Composable types.
After this operation, setindex!(a, val) will result in
setindex!(b, val) being executed.
link!(Scale, a => b)
"""
function link!{F <: Field}(::Type{F}, pair::Pair{ReactiveComposable, Composable})
    a, b = pair
    on(F, a) do val
        b[F] = val
    end
end


function on(F, object::ReactiveComposable, head, tail...)
    links = object[Links]
    args = (head, tail...)
    _fields = []; _args = []
    field, state = first(args), start(args)
    nomorefields = false
    for elem in args
        if !nomorefields && isa(elem, Type) && elem <: Field
            push!(_fields, elem)
        else
            nomorefields = true
            push!(_args, elem)
        end
    end
    args = (_args...,)
    fields = (_fields...,)
    for field in fields
        fieldlinks = get!(links, field, [])
        # adds a callback to the field
        push!(fieldlinks, (F, fields, args))
    end
end
