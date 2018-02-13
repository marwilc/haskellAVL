-- Alejandro Moreau
-- 27/01/2016

module PuntoXY(PuntoXY(Par), origen, retornarx, retornary, sumaXY, modx, mody, invertir) -- Nombre del modulo y sus funciones
where
data PuntoXY = Vacia | Par Int Int deriving Show -- Del modulo, se define una estructura con 2 opciones: Vacia (sin nada) o un Par que tiene 2 enteros

origen::PuntoXY -> Bool -- (x,y) = (0,0)?
origen Vacia = True
origen (Par x y) = x == 0 && y == 0

retornarx::PuntoXY -> Int -- (x,y) -> x
retornarx Vacia = 0
retornarx (Par x _) = x

retornary::PuntoXY -> Int -- (x,y) -> y
retornary Vacia = 0
retornary (Par _ y) = y

sumaXY::PuntoXY -> Int -- (x,y) -> x+y
sumaXY (Par x y) = x + y

modx::PuntoXY -> Int -> PuntoXY -- (x,y) -> (z,y)
modx (Par x y) xm = Par xm y

mody::PuntoXY -> Int -> PuntoXY -- (x,y) -> (x,z)
mody (Par x y) ym = Par x ym

invertir::PuntoXY -> PuntoXY -- (x,y) -> (y,x)
invertir (Par x y) = Par y x
