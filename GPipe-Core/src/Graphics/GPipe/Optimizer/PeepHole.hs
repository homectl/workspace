module Graphics.GPipe.Optimizer.PeepHole where

import           Control.Applicative                         (ZipList (..))
import           Control.Arrow                               ((&&&))
import qualified Data.List                                   as List
import           Data.Maybe                                  (listToMaybe)
import           Debug.Trace                                 (trace)
import           Graphics.GPipe.Optimizer.GLSL
import qualified Graphics.GPipe.Optimizer.StructuralEquality as StructuralEquality

pass :: GLSL -> GLSL
pass (GLSL v d) = GLSL v (map phTopDecl d)

maxLookahead :: Int
maxLookahead = 500

initWindowSize :: Int
initWindowSize = 10

-- | Minimum number of times a piece of code needs to appear for it to be
--   worth extracting into a function.
minRepeats :: Int
minRepeats = 3


phTopDecl :: TopDecl -> TopDecl
phTopDecl (FunDecl ty fn params body) =
  FunDecl ty fn params $ phStmts body
phTopDecl d = d


phStmts :: [Stmt] -> [Stmt]
phStmts ss =
  case findBody ss of
    Nothing -> ss
    Just body ->
      trace ("found one! length = " ++ show (length body) ++ "\n" ++ ppl ppStmt body) $
      deleteBody body ss


-- | Remove all occurrences of 'body' from 'ss'.
deleteBody :: [Stmt] -> [Stmt] -> [Stmt]
deleteBody body = go []
  where
    go acc [] = reverse acc
    go acc (s:ss) =
      if StructuralEquality.eqStmts (zip body ss)
        then go acc (drop (length body) ss)
        else go (s:acc) ss


findBody :: [Stmt] -> Maybe [Stmt]
findBody [] = Nothing
findBody (_:ss) =
  let
    -- Get a peep hole window of statements.
    window = take initWindowSize ss

    -- Try to find a similar set of statements somewhere in the
    -- lookahead range.
    matches =
      filter (\l -> not (null l) && StructuralEquality.eqStmts l)
      . map (zip window)
      . take maxLookahead
      . tail
      . List.tails
      $ ss

    -- If we found one, see how many more we find in the code.
    allRepeats =
      map fst
      . filter (\(_, l) -> not (null l) && StructuralEquality.eqStmts l)
      . map (id &&& zip window)
      . tail
      . List.tails
      $ ss

    -- If there are enough repeats to be worth extracting, try to maximise
    -- the amount of code extracted.
    maximised =
      transpose
      . takeWhile allEqual
      . transpose
      $ ss : allRepeats
  in
  if length window < initWindowSize
    then Nothing
    else case matches of
      -- No matches, continue looking.
      [] -> findBody ss
      -- Found one, but the number of repeats doesn't make it worth
      -- extracting into a function (minRepeats counts the first occurrence
      -- which is in the window and not in allRepeats).
      _:_ | length (take (minRepeats - 1) allRepeats) < minRepeats - 1 ->
        findBody ss
      _:_ | length (head maximised) < initWindowSize ->
        findBody ss
      -- Found one with several repeats, we'll extract this one.
      _:_ -> listToMaybe maximised


transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList


allEqual :: [Stmt] -> Bool
allEqual []     = True
allEqual (x:xs) = all (StructuralEquality.eqStmt x) xs
