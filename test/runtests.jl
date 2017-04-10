using FieldTraits, Quaternions, GeometryTypes, Colors
using Base.Test
import FieldTraits: @field, @composed, Fields, default

@field ImageData

@field SpatialOrder begin
    SpatialOrder = (1, 2)
end
@field Ranges

@field Rotation begin
    Rotation = Quaternion(1, 0, 0, 0)
end
@field Scale begin
    Scale = Vec3f0(1)
end
@field Position begin
    Position = Point3f0(0)
end
@composed type Transform
    Rotation
    Scale
    Position
end


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
img1 = Image(ImageData => data)
img2 = Image(ImageData => data, SpatialOrder => (2, 1))
img3 = Image(ImageData => data, SpatialOrder => :yx)
img4 = GrayImage(ImageData => data, SpatialOrder => :yx)

default_trans = Transform()
@test default_trans[Rotation] == Quaternion(1, 0, 0, 0)
@test default_trans[Scale] == Vec3f0(1)
@test default_trans[Position] == Point3f0(0)

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
