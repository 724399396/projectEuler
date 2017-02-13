import Data.Vector as V

triangle = V.map help $ V.enumFromTo 1 100000
  where help n = (n, n*(n+1) `quot` 2)
pentagonal = V.map help $ V.enumFromTo 1 100000
  where help n = (n, n*(3*n-1) `quot` 2)
hexagonal = V.map help $ V.enumFromTo 1 100000
  where help n = (n,n*(2*n-1))

res = do
  (i,t) <- triangle
  (j,p) <- V.filter ((==t).snd) $ V.takeWhile ((<=t).snd) pentagonal
  (k,h) <- V.filter ((==t).snd) $ V.takeWhile ((<=t).snd) hexagonal
  return ((i,j,k),t)
