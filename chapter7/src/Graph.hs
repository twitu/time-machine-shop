module Graph where

import           Control.Monad.Logic

graph2 :: [(Int, Int)]
graph2 = [(2013, 501), (501, 2558), (501, 1004), (1004, 501), (2013, 258)]

pathsL :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL edges start end =
  let e_paths = do
        (e_start, e_end) <- choices edges
        guard $ e_start == start
        subpath <- pathsL edges e_end end
        return $ start : subpath
  in  if start == end then return [end] `mplus` e_paths else e_paths

choices :: [a] -> Logic a
choices = msum . map return

pathsL' :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL' edges start end =
  let
    e_paths =
      (   choices edges
      >>= (\(e_start, e_end) ->
            (guard $ e_start == start)
              >>= (\_ ->
                    pathsL edges e_end end
                      >>= (\subpath -> return $ start : subpath)
                  )
          )
      )
  in  if start == end then return [end] `mplus` e_paths else e_paths

pathsLFair :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsLFair edges start end =
  let
    e_paths =
      (   choices edges
      >>- (\(e_start, e_end) ->
            (guard $ e_start == start)
              >>- (\_ ->
                    pathsL edges e_end end
                      >>- (\subpath -> return $ start : subpath)
                  )
          )
      )
  in  if start == end then return [end] `mplus` e_paths else e_paths
