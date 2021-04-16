module Graphics.GPipe.Optimizer.FunctionGenerator where

import           Graphics.GPipe.Optimizer.GLSL


makeFunction :: [Stmt] -> TopDecl
makeFunction _ = ProcDecl ProcMain [] []
