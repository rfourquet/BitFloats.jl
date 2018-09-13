# * BitFloats

module BitFloats

export Float80,  Inf80,  NaN80,
       Float128, Inf128, NaN128

import Base: *, +, -, /, eps, exponent_half, exponent_mask, exponent_one, floatmax, floatmin,
             precision, promote_rule, reinterpret, rem, round, sign_mask, significand_mask,
             typemax, typemin, uinttype, unsafe_trunc

using Base: llvmcall, uniontypes

using Core: BuiltinInts

import BitIntegers

import Random: rand
using  Random: AbstractRNG, CloseOpen01, CloseOpen12, SamplerTrivial


# * definitions

primitive type Float80  <: AbstractFloat 80  end
primitive type Float128 <: AbstractFloat 128 end

const WBF = Union{Float80,Float128}

BitIntegers.@define_integers 80

uinttype(::Type{Float80}) = UInt80
uinttype(::Type{Float128}) = UInt128

const llvmvars = ((Float80, "x86_fp80", "i80", "f80"), (Float128, "fp128", "i128", "f128"))


# * traits

sign_mask(::Type{Float80})            = 0x8000_0000_0000_0000_0000 % UInt80
exponent_mask(::Type{Float80})        = 0x7fff_0000_0000_0000_0000 % UInt80
exponent_one(::Type{Float80})         = 0x3fff_8000_0000_0000_0000 % UInt80
exponent_half(::Type{Float80})        = 0x3ffe_8000_0000_0000_0000 % UInt80
significand_mask(::Type{Float80})     = 0x0000_ffff_ffff_ffff_ffff % UInt80
explicit_bit(::Type{Float80}=Float80) = 0x0000_8000_0000_0000_0000 % UInt80
# non-implicit most significand bit of significand is actually stored for Float80

sign_mask(::Type{Float128})           = 0x8000_0000_0000_0000_0000_0000_0000_0000
exponent_mask(::Type{Float128})       = 0x7fff_0000_0000_0000_0000_0000_0000_0000
exponent_one(::Type{Float128})        = 0x3fff_0000_0000_0000_0000_0000_0000_0000
exponent_half(::Type{Float128})       = 0x3ffe_0000_0000_0000_0000_0000_0000_0000
significand_mask(::Type{Float128})    = 0x0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff

significand_bits(::Type{T}) where {T<:WBF} = trailing_ones(significand_mask(T))
exponent_bits(   ::Type{T}) where {T<:WBF} = sizeof(T)*8 - significand_bits(T) - 1
exponent_bias(   ::Type{T}) where {T<:WBF} = Int(exponent_one(T) >> significand_bits(T))
# exponent_bias is 16383 for both types

eps(     ::Type{Float80})  = reinterpret(Float80,  0x3fc0_8000_0000_0000_0000 % UInt80)
floatmin(::Type{Float80})  = reinterpret(Float80,  0x0001_8000_0000_0000_0000 % UInt80)
floatmax(::Type{Float80})  = reinterpret(Float80,  0x7ffe_ffff_ffff_ffff_ffff % UInt80)
typemin( ::Type{Float80})  = -Inf80
typemax( ::Type{Float80})  =  Inf80

eps(     ::Type{Float128}) = reinterpret(Float128, 0x3f8f_0000_0000_0000_0000_0000_0000_0000)
floatmin(::Type{Float128}) = reinterpret(Float128, 0x0001_0000_0000_0000_0000_0000_0000_0000)
floatmax(::Type{Float128}) = reinterpret(Float128, 0x7ffe_ffff_ffff_ffff_ffff_ffff_ffff_ffff)
typemin( ::Type{Float128}) = -Inf128
typemax( ::Type{Float128}) =  Inf128

const Inf80 = reinterpret(Float80,   0x7fff_8000_0000_0000_0000 % UInt80)
const NaN80 = reinterpret(Float80,   0xffff_c000_0000_0000_0000 % UInt80)

const Inf128 = reinterpret(Float128, 0x7fff_0000_0000_0000_0000_0000_0000_0000)
const NaN128 = reinterpret(Float128, 0xffff_8000_0000_0000_0000_0000_0000_0000)

precision(::Type{Float80})  = 64
precision(::Type{Float128}) = 113


# * conversions

# ** signedness

reinterpret(::Type{Unsigned}, x::Float80)  = reinterpret(UInt80, x)
reinterpret(::Type{Unsigned}, x::Float128) = reinterpret(UInt128, x)
reinterpret(::Type{Signed},   x::Float80)  = reinterpret(Int80, x)
reinterpret(::Type{Signed},   x::Float128) = reinterpret(Int128, x)


# ** from ints

for (F, f, i) = llvmvars
    for T = uniontypes(BuiltinInts)
        s = 8*sizeof(T)

        # T -> F
        itofp = T <: Signed ? :sitofp : :uitofp
        @eval begin
            (::Type{$F})(x::$T) = llvmcall(
                $"""
                %y = $itofp i$s %0 to $f
                %mi = bitcast $f %y to $i
                ret $i %mi
                """,
                $F, Tuple{$T}, x)

            promote_rule(::Type{$F}, ::Type{$T}) = $F
        end
        T === Bool && continue

        # F -> T
        fptoi = T <: Signed ? :fptosi : :fptoui
        @eval unsafe_trunc(::Type{$T}, x::$F) = llvmcall(
            $"""
            %x = bitcast $i %0 to $f
            %y = $fptoi $f %x to i$s
            ret i$s %y
            """,
            $T, Tuple{$F}, x)
    end
end


# ** from floats

for (F, f, i) = llvmvars
    for (S, s) = ((Float32, :float), (Float64, :double))
        @eval begin
            (::Type{$F})(x::$S) = Base.llvmcall(
                $"""
                %y = fpext $s %0 to $f
                %yi = bitcast $f %y to $i
                ret $i %yi
                """,
                $F, Tuple{$S}, x)

            (::Type{$S})(x::$F) = Base.llvmcall(
                $"""
                %x = bitcast $i %0 to $f
                %y = fptrunc $f %x to $s
                ret $s %y
                """,
                $S, Tuple{$F}, x)

            promote_rule(::Type{$F}, ::Type{$S}) = $F
        end
    end
    @eval begin
        (::Type{$F})(x::Float16) = $F(Float32(x))
        (::Type{Float16})(x::$F) = Float16(Float32(x))
        promote_rule(::Type{$F}, ::Type{Float16}) = $F
    end
end


# ** round

for (F, f, i, fn) = llvmvars
    # TODO: can be broken for Float128
    for (mode, llvmfun) = ((:ToZero, :trunc), (:Down, :floor),
                           (:Up, :ceil), (:Nearest, :rint))
        fun = "@llvm.$llvmfun.$fn"
        @eval round(x::$F, r::$(RoundingMode{mode})) = Base.llvmcall(
            ($"""declare $f $fun($f %Val)""",
             $"""
             %x = bitcast $i %0 to $f
             %y = call $f $fun($f %x)
             %z = bitcast $f %y to $i
             ret $i %z
             """), $F, Tuple{$F}, x)
    end
end


# * arithmetic

for (F, f, i) = llvmvars
    for (op, fop) = ((:*, :fmul), (:/, :fdiv), (:+, :fadd), (:-, :fsub), (:rem, :frem))
        @eval $op(x::$F, y::$F) = Base.llvmcall(
            $"""
            %x = bitcast $i %0 to $f
            %y = bitcast $i %1 to $f
            %m = $fop $f %x, %y
            %mi = bitcast $f %m to $i
            ret $i %mi
            """,
            $F, Tuple{$F,$F}, x, y)
    end
end

-(x::F) where {F<:WBF} = F(-0.0) - x


# * rand

rand(rng::AbstractRNG, ::SamplerTrivial{CloseOpen12{Float80}}) =
    reinterpret(Float80, rand(rng, UInt64) | exponent_one(Float80) | explicit_bit())

rand(rng::AbstractRNG, ::SamplerTrivial{CloseOpen12{Float128}}) =
    reinterpret(Float128, rand(rng, UInt128) & significand_mask(Float128) | exponent_one(Float128))

rand(rng::AbstractRNG, ::SamplerTrivial{CloseOpen01{T}}) where {T<:WBF} =
    rand(rng, CloseOpen12(T)) - one(T)


end # module
