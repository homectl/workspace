{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Graphics.GPipe.Optimizer where

import           Control.Monad                     (when)
import qualified Data.Text.Lazy                    as LT
import qualified Data.Text.Lazy.IO                 as IO
import qualified Graphics.GPipe.Optimizer.Deinline as Deinline
import           Graphics.GPipe.Optimizer.GLSL     (GLSL, parseGLSL,
                                                    parseShader, parseTest,
                                                    printShader)


optimizeShader :: LT.Text -> Either String LT.Text
-- optimizeShader = fmap printShader . parseShader
optimizeShader = fmap (printShader . optimize) . parseShader


optimize :: GLSL -> GLSL
optimize = (`const` id)
  -- . foldr (.) id (replicate 20 $ Deinline.pass Deinline.defaultConfig{Deinline.windowSize=5})
  -- . foldr (.) id (replicate 30 $ Deinline.pass Deinline.defaultConfig{Deinline.windowSize=10})
  . Deinline.pass Deinline.defaultConfig{Deinline.windowSize=10}


main :: IO ()
main = do
  putStrLn "Loading shader source..."
  -- inText <- IO.readFile "../large-shaders/lambdacnc.frag"
  -- inText <- IO.readFile "../large-shaders/lambdacnc.vert"
  -- inText <- IO.readFile "../large-shaders/lambdaray.frag"
  inText <- IO.readFile "../large-shaders/xax.frag"
  -- inText <- IO.readFile "../large-shaders/xax.vert"
  when False $ parseTest parseGLSL inText
  putStrLn "Parsing shader source..."
  case parseShader inText of
    Left err -> writeFile "../opt.glsl" $ "// Error\n" <> err
    Right ok -> do
      putStrLn "Optimizing shader..."
      IO.writeFile "../opt.glsl" $ printShader $ optimize ok
