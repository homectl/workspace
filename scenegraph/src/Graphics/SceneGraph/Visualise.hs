{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.SceneGraph.Visualise
  ( toDot
  , toSvg
  ) where

import qualified Data.GraphViz                   as GV
import qualified Data.GraphViz.Attributes.Colors as C
import qualified Data.GraphViz.Attributes.HTML   as H
import           Data.Maybe                      (catMaybes)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as LT
import           Data.Word                       (Word8)
import           Graphics.SceneGraph.Types
import           Linear                          (M44, V4 (..))


-------------------- Graph Visualisation --------------------

instance GV.Labellable (SceneNode g) where
  toLabelValue (SceneNode _ _ sd) = GV.toLabelValue sd

instance GV.Labellable SceneEdge where
  toLabelValue DefaultEdge = GV.toLabelValue ""

instance GV.Labellable (SceneData g) where
  toLabelValue Group               = GV.toLabelValue "Group"
  toLabelValue (Geode n _)         = GV.toLabelValue $ "Geode " ++ show n
  toLabelValue LOD                 = GV.toLabelValue "LOD"
  toLabelValue (MatrixTransform m) = GV.toLabelValue $ matrixToHtml m
  toLabelValue (Switch i)          = GV.toLabelValue $ "Switch " ++ show i
  toLabelValue (Material m)        = GV.toLabelValue $ materialToHtml m
  toLabelValue (Handler _ _)       = GV.toLabelValue "Handler"
  toLabelValue Light               = GV.toLabelValue "Light"
  toLabelValue Camera              = GV.toLabelValue "Camera"
  toLabelValue (Texture _)         = GV.toLabelValue "Texture"
  toLabelValue (Text t)            = GV.toLabelValue $ "Text " ++ T.unpack t


cell :: String -> H.Cell
cell text = H.LabelCell [] $ H.Text [H.Str $ LT.pack text]

colourCell :: V4 Float -> String -> H.Cell
colourCell col text = H.LabelCell [H.BGColor $ toRGB col] $ H.Text [H.Str $ LT.pack text]

toRGB :: V4 Float -> C.Color
toRGB (V4 r g b _) = C.RGB (w8 r) (w8 g) (w8 b)
  where
    w8 :: Float -> Word8
    w8 c = round $ c * 255

table :: [H.Row] -> H.Label
table rows = H.Table $ H.HTable
  { H.tableFontAttrs = Nothing
  , H.tableAttrs = []
  , H.tableRows = rows
  }

matrixToHtml :: M44 Float -> H.Label
matrixToHtml (V4 a b c d) = H.Table $ H.HTable
  { H.tableFontAttrs = Nothing
  , H.tableAttrs = []
  , H.tableRows = map (H.Cells . vectorToHtml) [a, b, c, d]
  }

vectorToHtml :: V4 Float -> [H.Cell]
vectorToHtml (V4 a b c d) = map tableCell [a, b, c, d]
  where
    tableCell v = cell $ show v

materialToHtml :: Phong -> H.Label
materialToHtml Phong{..} = table rows
  where
    colourToHtml name col = fmap (\v -> H.Cells [cell name, H.LabelCell [] $ table [H.Cells $ colourCell v "RGBA" : vectorToHtml v]]) col
    valueToHtml name val = fmap (\v -> H.Cells [cell name, cell $ show v]) val

    rows = catMaybes
      [ colourToHtml "Emission" phEmission
      , colourToHtml "Ambient" phAmbient
      , colourToHtml "Diffuse" phDiffuse
      , colourToHtml "Specular" phSpecular
      , valueToHtml "Shine" phShine
      , colourToHtml "Reflective" phReflective
      , valueToHtml "Reflectivity" phReflectivity
      , colourToHtml "Transparent" phTransparent
      , valueToHtml "Transparency" phTransparency
      ]


toDot :: Scene g -> FilePath -> IO FilePath
toDot (Scene sg _) = GV.runGraphviz (GV.graphToDot GV.quickParams sg) GV.Canon

toSvg :: Scene g -> FilePath -> IO FilePath
toSvg (Scene sg _) = GV.runGraphviz (GV.graphToDot GV.quickParams sg) GV.Svg
