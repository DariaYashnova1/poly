import Control.Monad (forM_)

data Square = Square (Double, Double) (Double, Double)
instance Show Square where
    show (Square (x ,y) (u ,v) ) = "(" ++ show x ++","++ show y ++")("++show u ++","++ show v++")"
-- Функция для заполнения кортежа
fillSquare :: Double -> Double -> Double -> Double -> Square
fillSquare x y u v = Square (x, y) (u, v)

zeroSquare :: Square
zeroSquare = fillSquare 0.0 0.0 0.0 1.0

-- Доступ к элементам кортежа
getX :: Square -> Double
getX (Square (x, _) (_ ,_)) = x

getY :: Square -> Double
getY (Square(_ ,y)(_, _)) = y

getU :: Square -> Double
getU (Square (_ ,_ )(u, _)) = u

getV :: Square -> Double
getV (Square (_, _) (_, v)) = v

printSq (Square (x ,y) (u ,v)) = do
     print ( "{" ++ show x ++", "++ show y ++"), ("++show u ++", "++ show v++"} ")

leftSquare:: Square -> Square
leftSquare (Square (x ,y) (u ,v)) = fillSquare (2*u-x+y-v)  (2*v-y+u-x)  (2*u-x+y-v+(u-x)/2+(y-v)/2)  (2*v-y+u-x+(u-x)/2+(v-y)/2)

rightSquare:: Square -> Square
rightSquare (Square (x ,y) (u ,v)) = fillSquare (2*u-x+v-y)  (2*v-y+x-u)  (2*u-x+v-y+(u-x)/2+(v-y)/2)  (2*v-y+x-u+(v-y)/2+(x-u)/2)

fractal :: Int -> Square -> [Square]
fractal 0 square = [square]
fractal n square = fractal (n - 1) (leftSquare square) ++ fractal (n - 1) (rightSquare square)
   
grFrac (0) square = [[square]]
grFrac n square= [fractal n square] ++  grFrac (n-1) square
