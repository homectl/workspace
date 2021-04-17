module Main (main) where

import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.IO             as IO
import           Data.Time.Clock               as Time (diffUTCTime,
                                                        getCurrentTime,
                                                        nominalDiffTimeToSeconds)
import qualified Graphics.GPipe.Optimizer      as Opt
import qualified Graphics.GPipe.Optimizer.DFG  as DFG
import qualified Graphics.GPipe.Optimizer.GLSL as GLSL
import qualified System.Environment            as Env

main :: IO ()
main = Env.getArgs >>= mapM_ process

parseShader :: LT.Text -> Either String (GLSL.GLSL ())
parseShader = GLSL.parseShader

process :: FilePath -> IO ()
process inFile = do
  putStrLn "Loading file..."
  inText <- IO.readFile inFile
  putStrLn "Parsing file..."
  case parseShader inText of
    Left err -> fail err
    Right ok -> do
      putStrLn "Generating data flow graph (opt.dot)..."
      let dfg = DFG.genDFG ok
      DFG.toDot "opt.dot" dfg
      -- DFG.toSvg dfg "../opt.svg"
      putStrLn "Optimizing shader (opt.glsl)..."
      s <- Time.getCurrentTime
      let outText = GLSL.printShader $ Opt.optimize ok
      IO.writeFile "opt.glsl" outText
      e <- Time.getCurrentTime
      let elapsed = Time.nominalDiffTimeToSeconds $ Time.diffUTCTime e s
      putStrLn $ "Optimizer took " <> show elapsed <> "s"
      let inLen = LT.length inText
          outLen = LT.length outText
          factor :: Float
          factor = fromIntegral inLen / fromIntegral outLen
      putStrLn $ "Size: " <> showKB inLen <> " -> "
                  <> showKB outLen <> ", " <> show factor <> "x reduction"


showKB :: (Show a, Integral a) => a -> String
showKB x | x > 1024 * 1024 = show (x `div` (1024 * 1024)) <> "MiB"
showKB x | x > 1024 = show (x `div` 1024) <> "KiB"
showKB x = show x <> "B"
