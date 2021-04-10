module Graphics.GPipe.GeometryStream (
    -- * The data type
    GeometryStream(),

    -- * Needed to use custom data types with the geometry shader.
    FragmentCreator(..),
    AnotherVertexInput(..),
    AnotherFragmentInput(..),
    GeometryExplosive(..),

    -- * Creating GeometryStream
    geometrize,
    generateAndRasterize,

    -- * Various GeometryStream operations   
    generativePoint,
    generativeLineStrip,
    generativeTriangleStrip,
    emitVertex,
    endPrimitive,
)
where

import Graphics.GPipe.Internal.GeometryStream
