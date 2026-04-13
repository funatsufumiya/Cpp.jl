module Cpp

export @cpp

using Libdl

function call_cpp(::Type{R}, ::Type{ArgTuple}, mangled::AbstractString, lib, args...) where {R, ArgTuple}
    libhandle = lib
    if isa(lib, AbstractString)
        libhandle = Libdl.dlopen(lib)
    end
    ptr = Libdl.dlsym(libhandle, mangled)
    return _cpp_call_impl(R, ArgTuple, ptr, args...)
end

@generated function _cpp_call_impl(::Type{R}, ::Type{ArgTuple}, ptr, args...) where {R, ArgTuple}
    tuple_expr = Expr(:tuple, ArgTuple.parameters...)
    rettype_node = QuoteNode(R)
    nargs = length(ArgTuple.parameters)
    arg_exprs = Expr[]
    for i = 1:nargs
        push!(arg_exprs, Expr(:ref, :args, i))
    end
    return Expr(:call, :ccall, :ptr, rettype_node, tuple_expr, arg_exprs...)
end

const SUBSTITUTION = [""; map(string, collect(0:9)); map(string, collect('A':'Z'))]  # FIXME more than 37 arguments?

# Allow calling of functions in C++ shared libraries.
# Usage: a = @cpp ccall((:mysymbol,mylib),...)
macro cpp(ex)
    # If you get "undefined symbol" errors, use nm or readelf to view
    # the names of the symbols in the library. Then use the resulting
    # information to help improve this macro!
    # Note: for global objects without class or namespace qualifiers
    # (e.g., global consts), you do not need to use this macro (it
    # will work with a plain ccall, even though it is a C++ library)
    msg = "@cpp requires a ccall((:mysymbol, mylib),...)  expression"
    if isa(ex,Expr) && ex.head == :call
        if ex.args[1] != :ccall
            error(msg)
        end

        # println("head: ", ex.head, " , args: ", ex.args)

        ex = Expr(:ccall, ex.args[2:end]...)
        
        # println("ex: ", ex)
        # println("isa: ", isa(ex,Expr))
        # println("ex.head: ", ex.head)
    end
    
    if !isa(ex,Expr) || ex.head != :ccall
        error(msg)
    end

    # Parse the library symbol's name and normalize the tuple
    exlib = ex.args[1]
    if !(isa(exlib,Expr) && exlib.head == :tuple && length(exlib.args) >= 2)
        error(msg)
    end
    # extract symbol node and library node
    symnode = exlib.args[1]
    libnode = exlib.args[2]
    # resolve the symbol name without unnecessary eval when possible
    if isa(symnode,Expr) && symnode.head == :quote
        symval = symnode.args[1]
    elseif isa(symnode,Symbol)
        symval = symnode
    else
        symval = eval(symnode)
    end
    fstr = string(symval)
    # GNU3-4 ABI
    fstr = string("_Z", length(fstr), fstr)

    # Parse the arguments to ccall and construct the parameter type string
    exargtypes = ex.args[3]
    if exargtypes.head != :tuple
        error(msg)
    end
    exargs = exargtypes.args
    pstr = ""
    symtable = (:Void,:Bool,:Cchar,:Char,:String,:Int,:Int8,:Uint8,:Int16,:Uint16,:Int32,:Cint,:Uint32,:Int64,:Uint64,:Float32,:Float64)
    # GNU3-4 ABI v.3 and v.4
    ptable =   ('v',  'b',  'c',   'w',  "Pc",        'i', 'a',  'h',   's',   't',    'i',   'i',  'j',    'l',   'm',    'f',     'd')
    msub = String[]
    for iarg = 1:length(exargs)
        thisarg = exargs[iarg]
        thisargm = ""
        while isa(thisarg,Expr) && thisarg.head == :curly && thisarg.args[1] == :Ptr
            thisargm = string(thisargm,'P')
            thisarg = thisarg.args[2]
        end
        matched = false
        for isym = 1:length(symtable)
            if thisarg == symtable[isym]
                matched = true
                thisargm = string(thisargm, ptable[isym])
                # Cchar is a special notation just for name mangling,
                # convert back to :Int8
                if thisarg == :Cchar
                    ex.args[3].args[iarg] = :Int8
                end
                break
            end
        end
        if matched
            if length(thisargm) > 1
                # Use substitution
                idx = indexin([thisargm], msub)[1]
                if idx != 0
                    pstr = string(pstr, "S"*SUBSTITUTION[idx]*"_")
                else
                    push!(msub, thisargm)
                    pstr = string(pstr, thisargm)
                end
            else
                pstr = string(pstr, thisargm)
            end
        else
            println(thisarg)
            error("@cpp: argument not recognized")
        end
    end
    mangled = string(fstr, pstr)
    rettype_node = esc(ex.args[2])
    argtuple_type = Expr(:curly, :Tuple, ex.args[3].args...)
    lib_node = (isa(libnode,Expr) || isa(libnode,Symbol)) ? esc(libnode) : QuoteNode(libnode)

    call_args = Any[rettype_node, argtuple_type, QuoteNode(mangled), lib_node]
    for ival = 4:length(ex.args)
        a = ex.args[ival]
        push!(call_args, (isa(a,Expr) || isa(a,Symbol)) ? esc(a) : a)
    end
    return Expr(:call, :call_cpp, call_args...)
end

end # module Cpp
