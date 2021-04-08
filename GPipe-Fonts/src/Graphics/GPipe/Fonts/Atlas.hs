{-# LANGUAGE TupleSections #-}
module Graphics.GPipe.Fonts.Atlas
  ( FontAtlasOptions (..)
  , TextStyle (..)
  , createFontAtlas
  , buildTextMesh
  , loadFontFile
  ) where

import           Control.Monad          (when)
import           Data.IORef             (IORef, modifyIORef, newIORef,
                                         readIORef, writeIORef)
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IM
import           Data.List              (foldl')
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as UV
import           Graphics.GPipe         hiding (toInt)
import           Graphics.Text.TrueType


type OutlineCurves = [[V2 Float]]

data ISize = ISize Int Int deriving Show

data IRect = IRect Int Int Int Int deriving Show

data AtlasTree = Tree IRect AtlasNode

data AtlasNode = Split AtlasTree AtlasTree | Taken | Free

-- | A dynamic font atlas.  Given a font and a rendering method, it manages the resources needed to bake letters on a texture on
-- demand and create the geometry to render text using this texture.
data FontAtlas = FontAtlas
    { atlasRenderer     :: ()                                 -- ^ The rendering pipeline used to bake glyphs.
    , atlasIndex        :: IORef (IntMap CharacterDescriptor) -- ^ An index of the characters already present in the atlas.
    , atlasGlyphs       :: IORef (IntMap GlyphDescriptor)     -- ^ An index of the glyphs already rendered to the atlas.
    , atlasTree         :: IORef AtlasTree                    -- ^ Data structure to track the free space on the texture.
    , atlasFont         :: Font                               -- ^ The font associated with this atlas.
    , atlasFontRenderer :: FontRenderer                       -- ^ The font renderer for the chosen rendering method.
    , atlasOptions      :: FontAtlasOptions                   -- ^ Additional options specified during creation.
    }

-- | Options specified during atlas creation.
data FontAtlasOptions = FontAtlasOptions
    { atlasSize          :: Int  -- ^ The resolution of the atlas texture.
    , atlasLetterScale   :: Int  -- ^ The number of atlas pixels per em unit.
    , atlasLetterPadding :: Int  -- ^ The extra padding around each entry in the atlas.  Distance field renderers use this value
                                 -- to scale the blending width.  Extra padding might be needed when mipmapping is used.
    }

-- | A font renderer to be used for baking glyphs into an atlas.  Predefined renderers can be found in the
-- "LambdaCube.Font.SimpleDistanceField" and "LambdaCube.Font.CompositeDistanceField" modules.
data FontRenderer = FontRenderer
    { frPipeline     :: FontAtlasOptions -> ()                                        -- ^ The rendering pipeline whose top-level target will be the
                                                                                      -- atlas itself.  The output sampler must be called @"atlas"@.
    , frClearSurface :: () -> IO ()                                                   -- ^ The action to clear the atlas after creating the pipeline.
    , frBakeGlyph    :: FontAtlas -> OutlineCurves -> V2 Float -> V2 Float -> IO ()   -- ^ The action to bake a glyph to the atlas.
    }

-- | Options for preparing a text mesh.
data TextStyle = TextStyle
    { textLetterSpacing :: Float  -- ^ Extra displacement applied to the advance values, given in em units.  Negative values allowed.
    , textLineHeight    :: Float  -- ^ The distance between the baselines of consecutive lines, given in em units.
    }

-- | Descriptor for a single character.
data CharacterDescriptor = CharacterDescriptor
    { cdGlyphs  :: [(M33 Float, Int)] -- ^ The glyphs making up the character along with the necessary transformation.
    , cdAdvance :: Float              -- ^ Amount to advance the current point with in em units.
    }

-- | Descriptor for a single glyph.
data GlyphDescriptor = GlyphDescriptor
    { cdUVBounds   :: V4 Float  -- ^ Bounds on the atlas: xMin, yMin, xMax, yMax.
    , cdQuadBounds :: V4 Float  -- ^ Quad bounds relative to the current point in em units.
    }

-- | Default options for the atlas.
defaultOptions :: FontAtlasOptions
defaultOptions = FontAtlasOptions
    { atlasSize = 1024
    , atlasLetterScale = 64
    , atlasLetterPadding = 2
    }

-- | Default text style.
defaultTextStyle :: TextStyle
defaultTextStyle = TextStyle
    { textLetterSpacing = 0
    , textLineHeight = 1.25
    }

-- | Create an atlas from a font, a rendering method, and some additional options.
createFontAtlas :: Font -> FontRenderer -> FontAtlasOptions -> IO FontAtlas
createFontAtlas font fontRenderer options = do
    let size = atlasSize options

    atlasIndexRef <- newIORef IM.empty
    atlasGlyphsRef <- newIORef IM.empty
    atlasTreeRef <- newIORef (emptyAtlasTree size)
    return $ FontAtlas
        { atlasRenderer = ()
        , atlasIndex = atlasIndexRef
        , atlasGlyphs = atlasGlyphsRef
        , atlasTree = atlasTreeRef
        , atlasFont = font
        , atlasFontRenderer = fontRenderer
        , atlasOptions = options
        }

-- | Render a character to the atlas.  Returns 'True' in case of success.  The main use of this function is to render the
-- necessary letters ahead of time so displaying text doesn't cause huge frame skips.
renderCharacter :: FontAtlas -> Char -> IO Bool
renderCharacter atlas @ FontAtlas { atlasFont = font } character = do
    index <- readIORef (atlasIndex atlas)
    if IM.member characterIndex index then return True else do
        glyphSuccesses <- V.forM glyphs $ \rawGlyph -> renderGlyph atlas rawGlyph
        let success = V.and glyphSuccesses
        when success $ modifyIORef (atlasIndex atlas) (IM.insert characterIndex newDescriptor)
        return success
  where
    characterIndex = fromEnum character
    newDescriptor = CharacterDescriptor
        { cdGlyphs = map processTransforms (V.toList glyphs)
        , cdAdvance = advance / emSize
        }
    (advance, glyphs) = getCharacterGlyphsAndMetrics font character
    emSize = fromIntegral (unitsPerEm font)
    processTransforms rawGlyph = (transform, glyphIndex)
      where
        scales = _rawGlyphCompositionScale rawGlyph
        glyphIndex = _rawGlyphIndex rawGlyph
        transform = foldl' composeScales identity scales
        composeScales mtx (CompositeScaling a b c d) = mtx' !*! mtx
          where
            mtx' = V3 (V3 a' b' 0) (V3 c' d' 0) (V3 e' f' 1)
            a' = toFloat a
            b' = toFloat b
            c' = toFloat c
            d' = toFloat d
            -- TODO: get scale, it's not in the RawGlyph
            e' = fromIntegral 0 * scaler a' c' / emSize
            f' = fromIntegral 0 * scaler c' d' / emSize
            toFloat x = fromIntegral x / 0x4000
            scaler v1 v2
                | abs (abs v1 - abs v2) <= (33 / 65536 :: Float) = 2 * vf
                | otherwise = vf
              where
                vf = max (abs v1) (abs v2)

renderGlyph :: FontAtlas -> RawGlyph -> IO Bool
renderGlyph atlas @ FontAtlas { atlasFont = font, atlasOptions = options } rawGlyph = do
    glyphs <- readIORef (atlasGlyphs atlas)
    if IM.member glyphIndex glyphs then return True else bakeGlyph
  where
    glyphIndex = _rawGlyphIndex rawGlyph
    rawCurves = _rawGlyphContour rawGlyph
    bakeGlyph = do
        let emSize = fromIntegral (unitsPerEm font)
            letterScale = fromIntegral (atlasLetterScale options)
            boundingBox [] = (0, 0, 0, 0)
            boundingBox vs = (minI xs, minI ys, maxI xs, maxI ys)
              where
                xs = map fst vs
                ys = map snd vs
                minI = fromIntegral . minimum
                maxI = fromIntegral . maximum
            (xMin, yMin, xMax, yMax) = boundingBox (UV.toList =<< rawCurves)
            padding = atlasLetterPadding options
            glyphRectSize = ISize (toInt (xMax - xMin) + 1 + padding * 2) (toInt (yMax - yMin) + 1 + padding * 2)
              where
                toInt v = ceiling (v * letterScale / emSize)
        tree <- readIORef (atlasTree atlas)
        let (tree', newRect) = addRectangle tree glyphRectSize
        case newRect of
            Nothing -> return False
            Just (IRect cx cy cw ch) -> do
                let texelScale = 2 / fromIntegral (atlasSize options)
                    toUV coord = (fromIntegral coord - 0.5) * (texelScale / 2)
                    toClip coord = toUV coord * 2 - 1
                    curves = map (tessellateBezierCurves . map toVec2 . UV.toList) rawCurves
                    toVec2 (x, y) = V2 (fromIntegral x) (fromIntegral y)
                frBakeGlyph (atlasFontRenderer atlas) atlas curves (V2 xMin yMin) (V2 (toClip cx) (toClip cy))

                writeIORef (atlasTree atlas) tree'
                let newDescriptor = GlyphDescriptor
                        { cdUVBounds = V4 (toUV cx) (toUV cy) (toUV (cx + cw)) (toUV (cy + ch))
                        , cdQuadBounds = V4 quadX quadY (quadX + fromIntegral cw / letterScale) (quadY + fromIntegral ch / letterScale)
                        }
                    quadX = xMin / emSize - fromIntegral padding / letterScale
                    quadY = yMin / emSize - fromIntegral padding / letterScale
                modifyIORef (atlasGlyphs atlas) (IM.insert glyphIndex newDescriptor)

                return True

tessellateBezierCurves :: [V2 Float] -> [V2 Float]
tessellateBezierCurves vs = makeBezierSection =<< extractControlPoints vs
  where
    extractControlPoints (v1:v2:vs'@(v3:_)) = (v1,v2,v3) : extractControlPoints vs'
    extractControlPoints _ = []
    makeBezierSection (v1, v2, v3) = go 0 1 []
      where
        go ta tb rest
            | turnMeasure < 0.997 = go ta tm (go tm tb rest)
            | otherwise = pm : pb : rest
          where
            turnMeasure = dot (normalize (pm - pa)) (normalize (pb - pm))
            tm = (ta + tb) * 0.5
            pa = evalBezier ta
            pb = evalBezier tb
            pm = evalBezier tm
        evalBezier t = ((t*t) * v3) + ((2*t*t') * v2) + ((t'*t') * v1)
          where
            t' = 1-t

-- | Build a mesh from a string and render all the characters needed that aren't yet present on the atlas.  Every @'\n'@ character
-- starts a new line.
buildTextMesh :: FontAtlas -> TextStyle -> String -> IO ([V2 Float], [V2 Float])
buildTextMesh atlas style string = do
    mapM_ (renderCharacter atlas) string
    index <- readIORef (atlasIndex atlas)
    glyphs <- readIORef (atlasGlyphs atlas)
    let letterSpacing = textLetterSpacing style
        lineHeight = textLineHeight style
        (rects, uvs) = go string (V2 0 0) [] []
          where
            go [] point rects uvs = (reverse rects, reverse uvs)
            go ('\n':chars) (V2 _ pointY) rects uvs = go chars (V2 0 pointY') rects uvs
              where
                pointY' = pointY - lineHeight
            go (char:chars) point@(V2 pointX pointY) rects uvs = case IM.lookup characterIndex index of
                Nothing -> go chars point rects uvs
                Just descriptor -> go chars (V2 pointX' pointY') (newRects ++ rects) (newUVs ++ uvs)
                  where
                    pointX' = pointX + cdAdvance descriptor + letterSpacing
                    pointY' = pointY
                    (newRects, newUVs) = unzip (map makeGlyphQuad (cdGlyphs descriptor))
                    makeGlyphQuad (transform, glyphIndex) = ((transform, rect), uv)
                      where
                        glyph = glyphs IM.! glyphIndex
                        rect = cdQuadBounds glyph + V4 pointX pointY pointX pointY
                        uv = cdUVBounds glyph
              where
                characterIndex = fromEnum char
        textMesh = (makeQuads rects, makeQuads (map (identity,) uvs))
        makeQuads = concatMap makeQuad
        makeQuad (mat, V4 x1 y1 x2 y2) = map transform [V2 x1 y1, V2 x2 y1, V2 x2 y2, V2 x2 y2, V2 x1 y2, V2 x1 y1]
          where
            transform (V2 x y) = V2 x' y'
              where
                V3 x' y' _ = V3 x y 1 *! mat
    return textMesh

emptyAtlasTree :: Int -> AtlasTree
emptyAtlasTree size = Tree (IRect 0 0 size size) Free

addRectangle :: AtlasTree -> ISize -> (AtlasTree, Maybe IRect)
addRectangle tree@(Tree tRect@(IRect tx ty tw th) node) rSize@(ISize w h) = case node of
    Split lTree rTree -> case lRect of
        Just _  -> (Tree tRect (Split lTreeNew rTree), lRect)
        Nothing -> (Tree tRect (Split lTree rTreeNew), rRect)
      where
        (lTreeNew, lRect) = addRectangle lTree rSize
        (rTreeNew, rRect) = addRectangle rTree rSize
    Taken -> (tree, Nothing)
    Free
        | w == tw && h == th -> (Tree tRect Taken, Just tRect)
        | w <= tw && h <= th -> (Tree tRect (Split lTreeNew rTreeNew), rectNew)
        | otherwise -> (tree, Nothing)
      where
        dw = tw - w
        dh = th - h
        (lTreeNew, rectNew) = addRectangle (Tree lRect Free) rSize
        rTreeNew = Tree rRect Free
        (lRect, rRect) = if dw > dh
            then (IRect tx ty w th, IRect (tx + w) ty dw th)
            else (IRect tx ty tw h, IRect tx (ty + h) tw dh)
