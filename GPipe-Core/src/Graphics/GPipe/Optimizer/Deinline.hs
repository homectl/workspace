{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Graphics.GPipe.Optimizer.Deinline where

import           Control.Applicative                         (ZipList (..))
import           Control.Arrow                               ((&&&))
import qualified Data.List                                   as List
import           Data.Maybe                                  (listToMaybe)
import           Debug.Trace                                 (trace)
import           Graphics.GPipe.Optimizer.ConstExpr          (ConstExprs,
                                                              collectConstExprs)
import qualified Graphics.GPipe.Optimizer.FunctionGenerator  as FunctionGenerator
import           Graphics.GPipe.Optimizer.GLSL
import qualified Graphics.GPipe.Optimizer.StructuralEquality as StructuralEquality


data Config = Config
  { maxLookahead :: Int
  -- ^ Maximum number of statements to look ahead for equality.
  --
  --   Increasing this potentially finds more de-inlining opportunities but also
  --   drastically increases the cost of not finding any. This number does not
  --   matter if we always find an opportunity quickly.

  , minRepeats   :: Int
  -- ^ Minimum number of times a piece of code needs to appear for it to be
  --   worth extracting into a function.

  , maxRepeats   :: Int
  -- ^ Maximum number of initial repeats to use for maximization. If we find
  --   enough, we're happy and stop looking. Most of the time we'll find fewer
  --   than 10, but sometimes a bit of code is repeated a lot which would slow
  --   down the algorithm significantly.

  , windowSize   :: Int
  -- ^ Number of statements in the sliding window.
  }


defaultConfig :: Config
defaultConfig = Config
  { maxLookahead = 200
  , minRepeats = 3
  , maxRepeats = 10
  , windowSize = 10
  }


pass :: Config -> GLSL -> GLSL
pass config (GLSL v d) = GLSL v (map (diTopDecl config) d)

diTopDecl :: Config -> TopDecl -> TopDecl
diTopDecl config (ProcDecl fn params body) =
  let ce = collectConstExprs body in
  ProcDecl fn params $ diStmts config ce body
diTopDecl _ d = d


diStmts :: Config -> ConstExprs -> [Stmt] -> [Stmt]
diStmts config ce ss =
  case findBody config ce ss of
    Nothing -> ss
    Just body ->
      let _newProc = pp ppTopDecl (FunctionGenerator.makeFunction body) in
      trace (
        "found one! length = " <> show (length body)
        -- <> "\n" <> ppl ppStmt body <> "\n\n"
        -- <> newProc
      ) $ deleteBody ce body ss


-- | Remove all occurrences of 'body' from 'ss'.
deleteBody :: ConstExprs -> [Stmt] -> [Stmt] -> [Stmt]
deleteBody ce body = go []
  where
    go acc [] = reverse acc
    go acc (s:ss) =
      if StructuralEquality.eqStmts ce (zip body ss)
        then go (s:acc) (drop (length body) ss)
        else go (s:acc) ss


findBody :: Config -> ConstExprs -> [Stmt] -> Maybe [Stmt]
findBody _ _ [] = Nothing
findBody Config{..} _ (_:ss) | length ss < windowSize = Nothing
findBody config@Config{..} ce (_:ss) =
  let
    -- Get a peep hole window of statements.
    window = take windowSize ss

    -- We'll iterate over all possible sub-programs from the current position.
    -- We drop the window size, which means we may miss opportunties within the
    -- window, which matters more if the window is very large, so running the
    -- algorithm with a smaller window size and more passes generally works
    -- better.
    tails =
      drop windowSize
      . List.tails
      $ ss

    -- We want to find similar statements and filter out the empty sub-program
    -- since the empty list is trivially equal to another empty list.
    isSimilar l = not (null l) && StructuralEquality.eqStmts ce l

    -- Try to find a similar set of statements to the window somewhere in the
    -- lookahead range.
    firstRepeat =
      List.find isSimilar
      . map (zip window)
      . take maxLookahead
      $ tails

    -- If we found one, see how many more we find in the code.
    --
    -- If we find enough, we're happy and stop looking. Most of the time we'll
    -- find fewer than 10, but sometimes a bit of code is repeated a lot which would
    -- slow down the algorithm.
    allRepeats =
      take maxRepeats
      . map fst
      . filter (isSimilar . snd)
      . map (id &&& zip window)
      $ tails

    -- If there are enough repeats to be worth extracting, try to maximise
    -- the amount of code extracted.
    maximised =
      transpose
      . takeWhile (allEqual ce)
      . transpose
      $ ss : allRepeats
  in
  case firstRepeat of
    -- No matches, continue looking.
    Nothing -> findBody config ce ss
    -- Found one, but the number of repeats doesn't make it worth
    -- extracting into a function (minRepeats counts the first occurrence
    -- which is in the window and not in allRepeats).
    Just _ | length (take (minRepeats - 1) allRepeats) < minRepeats - 1 ->
      findBody config ce ss
    -- Found one with several repeats, we'll extract this one.
    Just _ -> listToMaybe maximised


transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

-- | Check for each statement whether it's structurally equal to the first one.
allEqual :: ConstExprs -> [Stmt] -> Bool
allEqual _ []      = True
allEqual ce (x:xs) = all (StructuralEquality.eqStmt ce x) xs
