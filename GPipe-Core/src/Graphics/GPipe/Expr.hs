-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Expr
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- This module provides the DSL for shader operations in GPipe. The type @'S' x a@ is an opaque type that represents a value of type @a@ in a shader stage @x@, eg @S F Float@ means a
-- floating point value in a fragment stream.
--
-----------------------------------------------------------------------------

module Graphics.GPipe.Expr (
    -- * Atomic shader type
    S(),
    V, F,
    VFloat, VInt, VWord, VBool,
    FFloat, FInt, FWord, FBool,
    GGenerativeGeometry,

    -- * Type classes where the Prelude ones are lacking
    Convert(..),
    Integral'(..),
    Bits'(..),
    Real'(..),
    FloatingOrd(..),

    -- * Additional functions
    dFdx,
    dFdy,
    fwidth,

    -- * Shader control structures
    while,
    ifThen,
    ifThenElse,
    ifThenElse',
    ShaderBase(),
    ShaderType(..)
)
where

import           Graphics.GPipe.Internal.Expr
