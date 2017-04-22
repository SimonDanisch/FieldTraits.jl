using FieldTraits, Base.Test

"`zap(x)` sends `x` to oblivion" zap(x) = nothing

@testset "UsageError" begin
    ue = FieldTraits.UsageError(zap, 2)
    @test ue.message == "`zap(x)` sends `x` to oblivion\n"
    @test ue.value == 2
    ue = FieldTraits.UsageError("a custom error message", 2)
    @test ue.message == "a custom error message"
end

@field FIscale
@field FIposition
@field FInotfield
@composed type FIcomposed
    FIscale
    FIposition
end
@testset "fieldindex" begin
    @test FieldTraits.fieldindex(FIcomposed, FIposition) === (Val{2}(),)
    @test FieldTraits.fieldindex(FIcomposed, FInotfield) === (Val{0}(),)
end
