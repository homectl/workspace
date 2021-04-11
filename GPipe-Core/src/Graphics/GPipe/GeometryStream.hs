module Graphics.GPipe.GeometryStream (
    -- * The data type
    GeometryStream(),

    -- * Needed to use custom data types with the geometry shader.
    FragmentCreator(..),
    AnotherVertexInput(..),
    AnotherFragmentInput(..),
    GeometryExplosive(..),

    -- * Needed for generic functions.
    GeometryInput(..),

    -- * Creating GeometryStream
    geometrize,
    generateAndRasterize,

    -- * Various GeometryStream operations
    generativePoints,
    generativeLineStrip,
    generativeTriangleStrip,
    emitVertex,
    emitVertexPosition,
    emitVertexLayer,
    emitVertexPositionAndLayer,
    endPrimitive,
)
where

import Graphics.GPipe.Internal.GeometryStream
