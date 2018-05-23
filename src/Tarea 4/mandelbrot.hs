import System.Environment (getArgs)
import Control.Parallel.Strategies

type R = Double
data C = C { real :: R
           , imag :: R
           } deriving (Show)

instance Num C where
  c1 + c2 = C { real = real c1 + real c2
              , imag = imag c1 + imag c2}
  c1 * c2 = C { real = (real c1 * real c2) - (imag c1 * imag c2)
              , imag = (real c1 * imag c2) + (real c2 * imag c1)}

norma :: C -> R
norma c = real c ^ (2 :: Int) + imag c ^ (2 :: Int)

plano :: Int -> [C]
plano n = do
  x <- [0..n-1]
  y <- [0..n-1]
  let r = 4.0 / fromIntegral (n-1) * fromIntegral x - 2.0
  let i = 4.0 / fromIntegral (n-1) * fromIntegral y - 2.0
  return C {real= r, imag=i}

checkMandel :: C -> Bool
checkMandel c = all ((< 4.0) . norma) . take 200 $ l
  where l = C {real = 0.0, imag = 0.0} : map (\c' -> c + (c' * c')) l

mandelbrotSet :: [C] -> String
mandelbrotSet l = unwords $ do
  c <- l
  if checkMandel c then
    return "1"
  else return "0"

mandelbrotSetParallel :: [C] -> String
mandelbrotSetParallel l = unwords $
  withStrategy (parListChunk 64 rseq) $ do
    c <- l
    if checkMandel c then
      return "1"
    else return "0"

main :: IO ()
main = do
  [tam, mode] <- getArgs
  let mandelbrot_ppm = "P2\n" ++
                       tam ++ " " ++ tam ++ "\n" ++
                       "1\n" ++
                       if mode == "par" then
                         mandelbrotSetParallel . plano . read $ tam
                       else
                         mandelbrotSet . plano . read $ tam
  writeFile "mandelbrot.ppm" mandelbrot_ppm
