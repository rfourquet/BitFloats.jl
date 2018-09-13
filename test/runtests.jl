using BitFloats, Test

using BitFloats: BuiltinInts, UInt80, explicit_bit, exponent_half, exponent_mask,
                 exponent_one, sign_mask, significand_mask, uinttype, uniontypes

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

@testset "conversions" begin
    for F = (Float80, Float128)
        for T = (uniontypes(BuiltinInts)..., Float16, Float32, Float64)
            t = rand(T)
            @test F(t) isa F
            T == Bool && continue
            if T <: Integer
                @test unsafe_trunc(T, F(t)) isa T
            else
                @test T(F(t)) isa T
            end
            @test promote_type(F, T) == F
            @test one(F) + one(T) isa F
        end
        @test one(F) isa F
        @test zero(F) isa F
    end
end

@testset "arithmetic" begin
    for T = (Float80, Float128)
        n = rand(Int)
        for op = (*, /, +, -, rem)
            for randfun = (_rand, rand)
                a, b = randfun(T), randfun(T)
                r = op(a, b)
                @test r isa T
                @test op(a, n) isa T
            end
        end
    end
end

@testset "rand" begin
    for T = (Float80, Float128)
        x = rand(T)
        @test x isa T
        u = reinterpret(uinttype(T), x)
        @test u & sign_mask(T) == 0
        u2 = reinterpret(uinttype(T), (rand(T) + one(T)))
        u2 &= exponent_mask(T)
        if T == Float80
            u2 |= explicit_bit(T)
        end
        @test u2 == exponent_one(T)
    end
end
