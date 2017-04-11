using FieldTraits, Quaternions, Colors
using Base.Test
import FieldTraits: @field, @composed, @reactivecomposed, Fields, default

@field ImageData

@field SpatialOrder begin
    SpatialOrder = (1, 2) # default value for SpatialOrder (xy)
end
@field Ranges

@field Rotation begin
    Rotation = Quaternion(1, 0, 0, 0)
end
@field Scale begin
    Scale = (1, 1, 1) # Should rather be Vec, but GeometryTypes doesn't work on 0.6 right now
end
@field Position begin
    Position = (0, 0, 0) # should be Point
end
@composed type Transform
    Rotation
    Scale
    Position
end

# Cool: we can document a field once and for all types which use that field!! :)
"""
Ranges indicate, on what an otherwise dimensionless visualization should be mapped.
E.g. use Ranges to indicate that an image should be mapped to a certain range.
"""
Ranges

"""
Transforms a visual. You can set the scale, position and rotation.
You can do this directly via e.g. transform[Scale] = (1, 1, 1), or better, call:
scale!(trasnform, (1,1,1)). The latter is guaranteed to work, even if Transform
is implemented differently (e.g. it's just a matrix).
"""
Transform

@composed type Image
    ImageData
    Ranges
    Transform
    SpatialOrder::NTuple{2, Int}
end

# Do a custom convert for SpatialOrder. Parent is left untyped, since this should apply
# to all SpatialOrders in all parent composables.
# If you need to overwrite behaviour if SpatialOrder is part of another composable, you just need to type parent
function Base.convert(::Type{SpatialOrder}, parent, value)
    isa(value, NTuple{2, Int}) && return value
    if !(value in (:xy, :yx))
        error("Spatial order only accepts :xy or :yz as a value. Found: $value")
    end
    value == :xy ? (1, 2) : (2, 1)
end

function default(x, ::Type{Ranges})
    data = x[ImageData]
    s = get(x, SpatialOrder) # if SpatialOrder in x, gets that, if not gets default(x, SpatialOrder)
    (0:size(data, s[1]), 0:size(data, s[2]))
end

# Just for the lulz, lets create almost the same type, with slightly different convert behaviour
# TODO extend composed macro, to actually inherit from Image, without repeating Images fields
# This could e.g. be used to represent the same visual for different backends,
# which need slightly different conversion behaviour
@composed type GrayImage
    ImageData
    Ranges
    Transform
    SpatialOrder::NTuple{2, Int}
end
function Base.convert(::Type{ImageData}, ::Type{GrayImage}, img)
    Gray.(img)
end


data = rand(RGB, 17, 42)
# Set up like this, we get conversion behaviour and partial construction with default generation for free :)
# It's probably not fully optimized yet, but these kind of constructor should be fast to execute and most calculation should
# be completely statically inferable and might land on the stack! :)
img1 = Image(ImageData => data)
img2 = Image(ImageData => data, SpatialOrder => (2, 1))
img3 = Image(ImageData => data, SpatialOrder => :yx)
img4 = GrayImage(ImageData => data, SpatialOrder => :yx)

default_trans = Transform() # this should initialize with the defaults of the fields
@test default_trans[Rotation] == Quaternion(1, 0, 0, 0)
@test default_trans[Scale] == (1, 1, 1)
@test default_trans[Position] == (0, 0, 0)

@testset "From data constructor" begin
    @test img1[ImageData] == data
    @test img1[Ranges] == (0:size(data, 1), 0:size(data, 2))
    @test img1[Transform] == Transform()
    @test img1[SpatialOrder] == (1, 2)
end

@testset "From reversed spatial order" begin
    @test img2[ImageData] == data
    @test img2[Ranges] == (0:size(data, 2), 0:size(data, 1))
    @test img2[Transform] == Transform()
    @test img2[SpatialOrder] == (2, 1)
end


@testset "From symbol spatial order" begin
    @test img3[ImageData] == data
    @test img3[Ranges] == (0:size(data, 2), 0:size(data, 1))
    @test img3[Transform] == Transform()
    @test img3[SpatialOrder] == (2, 1)
end

@testset "Gray Image" begin
    @test img4[ImageData] == Gray.(data)
    @test img4[Ranges] == (0:size(data, 2), 0:size(data, 1))
    @test img4[Transform] == Transform()
    @test img4[SpatialOrder] == (2, 1)
end


@field Mouse begin
    Mouse = (0, 0)
end
@field Area begin
    Area = (0, 0, 0, 0)
end

@reactivecomposed type Window
    Mouse
    Area
end
@testset "Reactive Composed" begin
    x = Window()
    testval = (77, 77)
    FieldTraits.on(Mouse, x) do mouse
        testval = mouse
        return
    end
    x[Mouse] = (22, 2)
    @test testval == (22, 2)
end
