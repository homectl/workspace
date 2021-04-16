module Graphics.GPipe.Optimizer.FunctionGenerator where

import           Graphics.GPipe.Optimizer.GLSL


makeFunction :: [StmtAnnot a] -> TopDecl a
makeFunction _ = ProcDecl ProcMain [] []
