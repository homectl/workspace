module Language.GLSL.Optimizer.FunctionGenerator where

import           Language.GLSL.AST


makeFunction :: [StmtAnnot a] -> TopDecl a
makeFunction _ = ProcDecl ProcMain [] []
