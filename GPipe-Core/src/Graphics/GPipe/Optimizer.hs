{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Graphics.GPipe.Optimizer where

import qualified Data.Text.Lazy                    as LT
import qualified Data.Text.Lazy.IO                 as IO
import qualified Graphics.GPipe.Optimizer.DFG      as DFG
import           Graphics.GPipe.Optimizer.GLSL     (GLSL, parseShader,
                                                    printShader)
import qualified Graphics.GPipe.Optimizer.PeepHole as PeepHole


optimizeShader :: LT.Text -> Either String LT.Text
-- optimizeShader = fmap printShader . parseShader
optimizeShader = fmap (printShader . optimize) . parseShader


optimize :: GLSL -> GLSL
optimize = PeepHole.pass


main :: IO ()
main = do
  putStrLn "Loading shader source..."
  -- inText <- IO.readFile "../lambdacnc/generated-shaders/shader9.out.frag"
  inText <- IO.readFile "../shader1.out.frag"
  -- inText <- IO.readFile "../shader1.out.vert"
  -- parseTest parseGLSL inText
  case parseShader inText of
    Left err -> writeFile "../opt.vert" $ "// Error\n" <> err
    Right ok -> do
      putStrLn "Constructing DFG..."
      let dfg = DFG.genDFG ok
      DFG.toDot "../opt.dot" dfg
      -- DFG.toSvg dfg "../opt.svg"
      putStrLn "Optimizing shader..."
      IO.writeFile "../opt.vert" $ printShader $ optimize ok
