using BitFloats, Test
using BitFloats: UInt80, explicit_bit, exponent_half, exponent_mask, exponent_one, sign_mask,
                 significand_mask, uinttype

@testset "definitions" begin
    @test @isdefined Float80
    @test @isdefined UInt80
    @test @isdefined Float128
    @test sizeof(Float80)  == 10
    @test sizeof(UInt80)   == 10
    @test sizeof(Float128) == 16
    @test Float80  <: AbstractFloat
    @test UInt80   <: Unsigned
    @test Float128 <: AbstractFloat
    @test isprimitivetype(Float80)
    @test isprimitivetype(UInt80)
    @test isprimitivetype(Float128)
    @test uinttype(Float80)  == UInt80
    @test uinttype(Float128) == UInt128
end

# generate a random float in the whole range
function _rand(T::Type)
    r = rand(uinttype(T))
    s = r & significand_mask(T)
    e = r & exponent_mask(T)
    if T === Float80
        s >>= 1 # delete explicit bit
        if e != 0
            s |= explicit_bit(T)
        end
    end
    reinterpret(T, r & sign_mask(T) | e | s)
end

@testset "traits" begin
    @test explicit_bit() == explicit_bit(Float80) isa UInt80
    for T in (Float80, Float128)
        for ufun ∈ (sign_mask, exponent_mask, exponent_one, significand_mask)
            @test ufun(T) isa uinttype(T)
        end
        for ffun ∈ (eps, floatmin, floatmax, typemax)
            @test ffun(T) isa T
        end
        x = _rand(T)
        # @test floatmin(T) <= x <= floatmax(T)
        # @test nextfloat(T(1)) - T(1) == eps(T)
    end
    @test Inf80  isa Float80
    @test NaN80  isa Float80
    @test Inf128 isa Float128
    @test NaN128 isa Float128
    @test precision(Float80)  == 64
    @test precision(Float128) == 113
    # @test Inf80  == typemax(Float80)  == -typemin(Float80)
    # @test Inf128 == typemax(Float128) == -typemin(Float128)
end

@testset "arithmetic" begin
    for T = (Float80, Float128)
        for op = (*, /, +, -, rem)
            a, b = _rand(T), _rand(T)
            r = op(a, b)
            @test r isa T
        end
    end
end
