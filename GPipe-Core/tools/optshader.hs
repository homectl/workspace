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
      inText <- IO.readFile inFile
      case GLSL.parseShader inText of
        Left err -> fail err
        Right ok -> do
          let dfg = DFG.genDFG ok
          DFG.toDot "opt.dot" dfg
          -- DFG.toSvg dfg "../opt.svg"
          IO.writeFile "opt.vert" $ GLSL.printShader $ Opt.optimize ok
