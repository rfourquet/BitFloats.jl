using BitFloats, Test

@testset "definitions" begin
    @test @isdefined Float80
    @test @isdefined Float128
    @test sizeof(Float80)  == 10
    @test sizeof(Float128) == 16
    @test Float80  <: AbstractFloat
    @test Float128 <: AbstractFloat
end
