module Language.GLSL.Optimizer.FunctionGenerator where

import           Language.GLSL.Types


makeFunction :: [StmtAnnot a] -> TopDecl a
makeFunction _ = ProcDecl ProcMain [] []
