module Main (main) where

import qualified Data.Text.Lazy.IO             as IO
import qualified Graphics.GPipe.Optimizer      as Opt
import qualified Graphics.GPipe.Optimizer.DFG  as DFG
import qualified Graphics.GPipe.Optimizer.GLSL as GLSL
import qualified System.Environment            as Env

main :: IO ()
main = Env.getArgs >>= mapM_ process
  where
    process inFile = do
      putStrLn "Loading file..."
      inText <- IO.readFile inFile
      putStrLn "Parsing file..."
      case GLSL.parseShader inText of
        Left err -> fail err
        Right ok -> do
          putStrLn "Generating data flow graph (opt.dot)..."
          let dfg = DFG.genDFG ok
          DFG.toDot "opt.dot" dfg
          -- DFG.toSvg dfg "../opt.svg"
          putStrLn "Optimizing shader (opt.glsl)..."
          IO.writeFile "opt.glsl" $ GLSL.printShader $ Opt.optimize ok
