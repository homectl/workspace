{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Graphics.GPipe.Optimizer where

import           Control.Monad                     (when)
import           Data.Attoparsec.ByteString.Char8  (Parser)
import qualified Data.Text.Lazy                    as LT
import qualified Data.Text.Lazy.IO                 as IO
import qualified Graphics.GPipe.Optimizer.Deinline as Deinline
import           Graphics.GPipe.Optimizer.GLSL     (Annot, GLSL, parseGLSL,
                                                    parseShader, parseTest,
                                                    printShader)
import qualified Graphics.GPipe.Optimizer.Liveness as Liveness


optimizeShader :: LT.Text -> Either String LT.Text
-- optimizeShader = fmap printShader . parse
optimizeShader = fmap (printShader . optimize) . parse


parse :: LT.Text -> Either String (GLSL ())
parse = parseShader

optimize :: Annot a => GLSL a -> GLSL a
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
  -- inText <- IO.readFile "../large-shaders/small.vert"
  when False $ parseTest (parseGLSL :: Parser (GLSL ())) inText
  putStrLn "Parsing shader source..."
  case parse inText of
    Left err -> writeFile "../opt.glsl" $ "// Error\n" <> err
    Right ok -> do
      putStrLn "Computing liveness..."
      let ls = Liveness.computeLiveness ok
      putStrLn "Optimizing shader..."
      IO.writeFile "../opt.glsl" $ printShader $ optimize ls
