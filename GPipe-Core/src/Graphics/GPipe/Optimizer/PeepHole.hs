module Graphics.GPipe.Optimizer.PeepHole where

import           Control.Applicative                         (ZipList (..))
import           Control.Arrow                               ((&&&))
import qualified Data.List                                   as List
import           Data.Maybe                                  (listToMaybe)
import           Debug.Trace                                 (trace)
import           Graphics.GPipe.Optimizer.ConstExpr          (ConstExprs,
                                                              collectConstExprs)
import           Graphics.GPipe.Optimizer.GLSL
import qualified Graphics.GPipe.Optimizer.StructuralEquality as StructuralEquality


pass :: Int -> GLSL -> GLSL
pass winSize (GLSL v d) = GLSL v (map (phTopDecl winSize) d)

maxLookahead :: Int
maxLookahead = 500

-- | Minimum number of times a piece of code needs to appear for it to be
--   worth extracting into a function.
minRepeats :: Int
minRepeats = 3


phTopDecl :: Int -> TopDecl -> TopDecl
phTopDecl winSize (FunDecl ty fn params body) =
  let ce = collectConstExprs body in
  FunDecl ty fn params $ phStmts winSize ce body
phTopDecl _ d = d


phStmts :: Int -> ConstExprs -> [Stmt] -> [Stmt]
phStmts winSize ce ss =
  case findBody winSize ce ss of
    Nothing -> ss
    Just body ->
      trace ("found one! length = " ++ show (length body)) $
      deleteBody ce body ss


-- | Remove all occurrences of 'body' from 'ss'.
deleteBody :: ConstExprs -> [Stmt] -> [Stmt] -> [Stmt]
deleteBody ce body = go []
  where
    go acc [] = reverse acc
    go acc (s:ss) =
      if StructuralEquality.eqStmts ce (zip body ss)
        then go (s:acc) (drop (length body) ss)
        else go (s:acc) ss


findBody :: Int -> ConstExprs -> [Stmt] -> Maybe [Stmt]
findBody _ _ [] = Nothing
findBody winSize ce (_:ss) =
  let
    -- Get a peep hole window of statements.
    window = take winSize ss

    -- Try to find a similar set of statements somewhere in the
    -- lookahead range.
    matches =
      filter (\l -> not (null l) && StructuralEquality.eqStmts ce l)
      . map (zip window)
      . take maxLookahead
      . tail
      . List.tails
      $ ss

    -- If we found one, see how many more we find in the code.
    allRepeats =
      map fst
      . filter (\(_, l) -> not (null l) && StructuralEquality.eqStmts ce l)
      . map (id &&& zip window)
      . tail
      . List.tails
      $ ss

    -- If there are enough repeats to be worth extracting, try to maximise
    -- the amount of code extracted.
    maximised =
      transpose
      . takeWhile (allEqual ce)
      . transpose
      $ ss : allRepeats
  in
  if length window < winSize
    then Nothing
    else case matches of
      -- No matches, continue looking.
      [] -> findBody winSize ce ss
      -- Found one, but the number of repeats doesn't make it worth
      -- extracting into a function (minRepeats counts the first occurrence
      -- which is in the window and not in allRepeats).
      _:_ | length (take (minRepeats - 1) allRepeats) < minRepeats - 1 ->
        findBody winSize ce ss
      _:_ | length (head maximised) < winSize ->
        findBody winSize ce ss
      -- Found one with several repeats, we'll extract this one.
      _:_ -> listToMaybe maximised


transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList


allEqual :: ConstExprs -> [Stmt] -> Bool
allEqual _ []      = True
allEqual ce (x:xs) = all (StructuralEquality.eqStmt ce x) $ take 1 $ xs
