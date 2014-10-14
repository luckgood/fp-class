{-# LANGUAGE EmptyDataDecls #-}


module GrahamScan where
import Data.Function (on) 
import Data.List (sortBy, minimumBy, delete)
-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point Double Double deriving (Show,Eq,Ord)

{-
2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
этих трёх возможностей определить специальный тип Direction.
-}

data Direction = L | S | R deriving (Show, Eq)

{-
3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
определить несколько вспомогательных функций.
-}

isLSR :: Point -> Point -> Point -> Direction
isLSR (Point x1 y1) (Point x2 y2) (Point x3 y3) 
    | d > 0 = L
	| d < 0 = R
    | otherwise = S
	where d = ((x2-x1)*(y3-y1) - (y2-y1)*(x3-x1))

directions :: [Point] -> [Direction]
directions [] = []
directions [x] = []
directions [x,y] = []
directions (x:y:z:xs) = isLSR x y z : directions(y:z:xs)  

{-
4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
(Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

minp :: [Point] -> Point
minp ps = minimumBy (compare `on` \(Point x y) -> (y, x)) ps 

polarAt :: Point -> Point -> Double
polarAt (Point x0 y0) (Point x1 y1)
    | x0 == x1 && y0 == y1 = 0
	| otherwise = atan $ dy/dx
    where 
	dx = x1-x0
	dy = y1-y0

delp :: Point -> [Point] -> [Point]	
delp x0 = delete x0

sortPolar :: Point -> [Point] -> [Point]
sortPolar x0 = sortBy (compare `on` polarAt x0) 

graham_scan :: [Point] -> [Point]
graham_scan = undefined
{-
5. Приведите несколько примеров работы функции graham_scan.
-}
