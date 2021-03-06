{- Marwil Campos
-- Jhoan Moreno
-- 11/02/2018 -}

{- Modulo arbol AVL mas sus funciones predeterminadas-}
module ArbolAVL(ArbolAVL(Nulo, Nodo), nulo, esNulo, crearAVL, raiz, izq, der, numNodos, esHoja, insertarAVL, eliminarAVL, buscarAVL, alturaAVL, balancear)
where -- en el modulo se especifican dos estructuras una esructura Nulo (Arbol Nulo) y un Nodo con dos sub arboles.
data ArbolAVL t = Nulo | Nodo t (ArbolAVL t) (ArbolAVL t) Int deriving (Show)

{- Crea un Arbol Nulo -}
nulo::ArbolAVL t -> ArbolAVL t
nulo Nulo = Nulo

{- Verifica si es un arbol Nulo-}
esNulo::ArbolAVL t -> Bool
esNulo Nulo = True
esNulo _ = False

{-Crear AVL a partir de una lista de elementos -}
crearAVL::(Ord t) => [t] -> ArbolAVL t
crearAVL [] = Nulo
crearAVL (x:y) = crear (Nodo x Nulo Nulo 0) y
                  where
                      crear tree [] = tree
                      crear tree (x:y) = crear (balancear (insertarAVL tree x)) y

{-Funcion que devuelve la raiz de un arbol-}
raiz::ArbolAVL t -> t
raiz (Nodo root _ _ _) = root

{-Funcion que obtiene el hijo izquierdo de un arbolAVl-}
izq::ArbolAVL t -> ArbolAVL t
izq (Nodo _ lt _ _) = lt

{-Funcion que obtiene el hijo derecho de un arbolAVl-}
der::ArbolAVL t -> ArbolAVL t
der (Nodo _ _ rt _) = rt

{-Funcion que retorna el numero de nodos de un ArbolAVL-}
numNodos::ArbolAVL t -> Int
numNodos Nulo = 0
numNodos (Nodo root lt rt _)
          | esNulo(lt) && esNulo(rt) = 1
          | esNulo(lt) = numNodos(rt) + 1
          | esNulo(rt) = numNodos(lt) + 1
          | not(esNulo(lt) && esNulo(rt)) = numNodos(lt) + numNodos(rt) + 1

{-Funcion que verifica si un nodo es Hoja-}
esHoja::ArbolAVL t -> Bool
esHoja Nulo = True
esHoja (Nodo root lt rt _) = esNulo(lt) && esNulo(rt)

{-Funcion que busca un elemento en el arbol-}
buscarAVL::(Ord t) => ArbolAVL t -> t -> ArbolAVL t
buscarAVL Nulo x = Nulo
buscarAVL (Nodo root lt rt hight) x
            | x == root = (Nodo root lt rt hight)
            | x  < root = buscarAVL lt x
            | x  > root = buscarAVL rt x

{-Funcion que retorna la altura de un arbolAVL-}
alturaAVL::(Ord t) => ArbolAVL t -> Int
alturaAVL Nulo = -1
alturaAVL (Nodo root lt rt _) = max (altura lt 1) (altura rt 1)
            where
                altura Nulo _ = 0
                altura t h
                  | esHoja t = h
                  | not (esNulo (izq t)) = altura (izq t) (h + 1)
                  | not (esNulo (der t)) = altura (der t) (h + 1)

{- Funcion que inserta un elemento al arbolAVl -}
insertarAVL::(Ord t) => ArbolAVL t -> t -> ArbolAVL t
insertarAVL Nulo e = Nodo e Nulo Nulo 0
insertarAVL (Nodo root lt rt hight) e
              | e == root = (Nodo root lt rt hight)
              | e  > root = (Nodo root lt (insertarAVL(rt) e) hight)
              | e  < root = (Nodo root (insertarAVL(lt) e) rt hight)

{-Funcion que inserta un sub arbol avl-}
insertarSubTreeAVL::(Ord t) => ArbolAVL t -> ArbolAVL t-> ArbolAVL t
insertarSubTreeAVL Nulo Nulo = Nulo
insertarSubTreeAVL Nulo (Nodo rooty ly ry highty) = (Nodo rooty ly ry highty)
insertarSubTreeAVL (Nodo rootx lx rx hightx) Nulo = (Nodo rootx lx rx hightx)
insertarSubTreeAVL (Nodo rootx lx rx hightx) (Nodo rooty ly ry highty)
            | rooty == rootx = (Nodo rootx lx rx hightx)
            | rooty  > rootx = (Nodo rootx lx (insertarSubTreeAVL rx (Nodo rooty ly ry highty)) sumHight)
            | rooty  < rootx = (Nodo rootx (insertarSubTreeAVL lx (Nodo rooty ly ry highty)) rx sumHight )
            where
              sumHight = hightx + highty

{- Funcion que elimina un elemento del arbolAVl -}
eliminarAVL::(Ord t) => ArbolAVL t -> t -> ArbolAVL t
eliminarAVL Nulo e = Nulo
eliminarAVL (Nodo root lt rt hight) e
              | e == root = Nulo
              | e  > root = (Nodo root lt (eliminarAVL rt e) hight)
              | e  < root = (Nodo root (eliminarAVL lt e) rt hight)

{-Funcion que balancea el arbol avl-}
balancear:: (Ord t) => ArbolAVL t -> ArbolAVL t
balancear Nulo = Nulo
balancear (Nodo root Nulo Nulo h) = Nodo root Nulo Nulo h
balancear (Nodo root lt rt h)
            | abs (caso) < 2 = Nodo root lt rt h
            | caso ==  2 &&  subLt == -1 = (Nodo (raiz (der lt)) (eliminarAVL lt (raiz (der lt))) (Nodo root (der(der lt)) rt h) h) -- rotacion doble de izquierda a derecha
            | caso == -2 &&  subRt ==  1 = (Nodo (raiz (izq rt)) (Nodo root lt (izq(izq rt)) h) (eliminarAVL rt (raiz(izq rt))) h) -- rotacion doble de derecha a izquierda
            | caso ==  2 &&  subLt /= -1 = (Nodo (raiz lt) (izq lt) (Nodo root (der lt) rt h) h)  -- rotacion simple hacia la derecha
            | caso == -2 &&  subRt /=  1 = (Nodo (raiz rt) (Nodo root lt (izq rt) h) (der rt) h)  -- rotacion simple hacia la izquierda

            where
              caso  = diferencia(Nodo root lt rt h)
              subLt = diferencia(lt)
              subRt = diferencia(rt)

{-hace la diferencia de las alturas de los hijos del arbol-}
diferencia ::(Ord t) => ArbolAVL t -> Int
diferencia Nulo = 0
diferencia (Nodo root lt rt h) = (alturaAVL lt) - (alturaAVL rt)
