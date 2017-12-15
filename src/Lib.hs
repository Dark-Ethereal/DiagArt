{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib
    ( someFunc
    , gray
    , printCode
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.List                    (transpose)
import           Data.List.Split              (chunksOf)
import           Data.Maybe                   (catMaybes)
import           Data.Monoid                  (mconcat)
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (gray)


someFunc :: IO ()
someFunc = mainWith (example :: Diagram B)

printCode :: [[Bool]] -> IO ()
printCode = putStrLn . unlines . fmap (unwords . fmap show)

gray 0 = [[]]
gray n = map (False:) g ++ map (True:) (reverse g)
   where g = gray (n-1)

f 0 = [False]
f 1 = [True]
f n = (f . fst $ dr) ++ (f . snd $ dr)
  where dr = divMod n 2

f1 False = [False, False, False]
f1 True  = [True, True, True]


f2 False = [False, False, False]
f2 True  = [True, False, True]

f3 :: [[Bool]] -> [[Bool]]
f3 []     = []
f3 (x:xs) = (x >>= f2) : ((>>= f1) <$> (x:xs))

f4 = ((iterate f3 [[True, False, True, False, True, False]]) !!)


rings n = mkRingsDia . map ringOffsets . transpose . gray $ n
  where ringOffsets :: [Bool] -> [(Direction V2 Double, Angle Double)]
        ringOffsets = map l2t . chunksOf 2 . findEdges . zip [rotate α xDir | α <- [0 @@ turn, 1/(2^n) @@ turn .. fullTurn]]
        l2t [x,y] =  (x, angleBetweenDirs x y)
        l2t [x]   = (x, angleBetweenDirs x xDir) -- arc angle will never be > fullturn ^/ 2

findEdges :: Eq a => [(Direction V2 Double, a)] -> [Direction V2 Double]
findEdges = catMaybes . (zipWith edge <*> tail)
   where edge (_,c1) (a,c2) | c1 /= c2  = Just a
                            | otherwise = Nothing

mkRingsDia = mconcat . zipWith mkRingDia [2,3..]
  where mkRingDia r = lwL 1.05 . mconcat . map (strokeP . scale r . uncurry arc)

example = pad 1.1 (rings 9)

fib 0 = 0
fib 1 = 1
fib 😂 = fib (😂-1) fib (😂-2)
