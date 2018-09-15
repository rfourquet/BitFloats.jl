using BitFloats, Test

using BitFloats: BuiltinInts, Int80, UInt80, explicit_bit, exponent_half, exponent_mask,
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
        for ffun ∈ (eps, floatmin, floatmax, typemin, typemax)
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
    @test Inf80  == typemax(Float80)  == -typemin(Float80)
    @test Inf128 == typemax(Float128) == -typemin(Float128)
    @test isnan(NaN80)
    @test isnan(NaN128)
    @test !isnan(_rand(Float80)) # very unlikely to fail
    @test !isnan(_rand(Float128))
    @test isinf(Inf80)
    @test isinf(Inf128)
    @test !isinf(_rand(Float80))
    @test !isinf(_rand(Float128))
end

@testset "conversions" begin
    for F = (Float80, Float128)
        for T = (uniontypes(BuiltinInts)..., Float16, Float32, Float64, Float80, Float128)
            t = rand(T)
            @test F(t) isa F
            T == Bool && continue
            if T <: Integer
                @test unsafe_trunc(T, F(t)) isa T
                @test promote_type(F, T) == F
            else
                @test T(F(t)) isa T
                @test isnan(F(T(NaN)))
                @test isnan(T(F(NaN)))
                @test isinf(F(T(Inf)))
                @test isinf(T(F(Inf)))
                @test isinf(F(T(-Inf)))
                @test isinf(T(F(-Inf)))

                R = sizeof(T) < sizeof(F) ? F : T
                @test promote_type(F, T) == R
                @test F(zero(T)) == T(zero(F)) == 0
            end
            @test one(F) + one(T) === promote_type(F, T)(2)
        end
        @test one(F) isa F
        @test zero(F) isa F
        x = _rand(F)
        @test reinterpret(Unsigned, x) === reinterpret(uinttype(F), x)
        @test reinterpret(Signed, x) === reinterpret(F == Float80 ? Int80 : Int128, x)
    end
end

@testset "round" begin
    for F = (Float80,) # broken for Float128
        @test round(F(1.2), RoundToZero) == F(1.0)
        @test round(F(1.2), RoundNearest) == F(1.0)
        @test round(F(1.2), RoundDown) == F(1.0)
        @test round(F(1.2), RoundUp) == F(2.0)
        @test round(F(1.8), RoundToZero) == F(1.0)
        @test round(F(1.8), RoundNearest) == F(2.0)
        @test round(F(1.8), RoundDown) == F(1.0)
        @test round(F(1.8), RoundUp) == F(2.0)
    end
end

@testset "comparisons" begin
    for F = (Float80, Float128)
        x, y = _rand(F), _rand(F)
        for op = (==, !=, <, <=, >, >=, isless, isequal)
            @test op(x, y) isa Bool
        end
        @test F(1) == F(1)
        @test F(1) != F(2)
        @test F(1) <  F(2)
        @test F(1) <= F(2)
        @test F(1) <= F(1)
        @test F(2) >  F(1)
        @test F(2) >= F(1)
        @test F(1) >= F(1)
        @test (x == y) == !(x != y)
        @test !(F(1) == F(2))
        @test !(F(1) != F(1))
        @test isequal(x, x)
        @test isequal(y, y)
        @test isequal(x, y) || isless(x, y) || isless(y, x)
        N = F(NaN)
        @test N != N
        @test isequal(N, N)
        @test !(N == N)
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
        x = _rand(T)
        @test -x  isa T
        @test -(-x) == x
        @test x >= 0 ? x == abs(x) : x == -abs(x)
        @test abs(T(-1)) == T(1)
        @test abs(T(1)) == T(1)
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
