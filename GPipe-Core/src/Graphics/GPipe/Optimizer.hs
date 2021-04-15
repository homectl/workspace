{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Graphics.GPipe.Optimizer where

import           Control.Monad                     (when)
import qualified Data.Text.Lazy                    as LT
import qualified Data.Text.Lazy.IO                 as IO
import qualified Graphics.GPipe.Optimizer.DFG      as DFG
import           Graphics.GPipe.Optimizer.GLSL     (GLSL, parseGLSL,
                                                    parseShader, parseTest,
                                                    printShader)
import qualified Graphics.GPipe.Optimizer.PeepHole as PeepHole


optimizeShader :: LT.Text -> Either String LT.Text
-- optimizeShader = fmap printShader . parseShader
optimizeShader = fmap (printShader . optimize) . parseShader


optimize :: GLSL -> GLSL
optimize =
  PeepHole.pass 10
  . PeepHole.pass 20
  . PeepHole.pass 30
  . PeepHole.pass 30
  . PeepHole.pass 60


main :: IO ()
main = do
  putStrLn "Loading shader source..."
  inText <- IO.readFile "../large-shaders/lambdacnc.frag"
  -- inText <- IO.readFile "../large-shaders/lambdacnc.vert"
  -- inText <- IO.readFile "../large-shaders/lambdaray.frag"
  -- inText <- IO.readFile "../large-shaders/xax.frag"
  -- inText <- IO.readFile "../large-shaders/xax.vert"
  when False $ parseTest parseGLSL inText
  putStrLn "Parsing shader source..."
  case parseShader inText of
    Left err -> writeFile "../opt.glsl" $ "// Error\n" <> err
    Right ok -> do
      putStrLn "Constructing DFG..."
      let dfg = DFG.genDFG ok
      DFG.toDot "../opt.dot" dfg
      -- DFG.toSvg dfg "../opt.svg"
      putStrLn "Optimizing shader..."
      IO.writeFile "../opt.vert" $ printShader $ optimize ok
