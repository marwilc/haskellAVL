{- Marwil Campos
-- Jhoan Moreno
-- 11/02/2018 -}

{- Modulo arbol AVL mas sus funciones predeterminadas-}
module ArbolAVL(ArbolAVL(Nulo, Nodo), nulo, esNulo, crearAVL, raiz, izq{-, der, numNodos, esHoja, insertarAVL, eliminarAVL, buscarAVL, alturaAVL, balancear-})
where -- en el modulo se especifican dos estructuras una esructura Nulo (Arbol Nulo) y un Nodo con dos sub arboles.
data ArbolAVL t = Nulo | Nodo t (ArbolAVL t) (ArbolAVL t) deriving (Show)

{- Crea un Arbol Nulo -}
nulo::ArbolAVL t -> ArbolAVL t
nulo Nulo = Nulo

{- Verifica si es un arbol Nulo-}
esNulo::ArbolAVL t -> Bool
esNulo Nulo = True
esNulo _ = False

{-Crear AVL a partir de una lista de elementos -}
crearAVL::ArbolAVL t -> ArbolAVL t
crearAVL (Nodo e t1 t2) = Nodo e t1 t2

{-Funcion que devuelve la raiz de un arbol-}
raiz::ArbolAVL t -> t
raiz (Nodo root t1 t2) = root

{-Funcion que obtiene el hijo izquierdo de un arbolAVl-}
izq::ArbolAVL t -> ArbolAVL t
izq (Nodo root t1 _) = t1

{-Funcion que obtiene el hijo derecho de un arbolAVl-}
der::ArbolAVL t -> ArbolAVL t
der (Nodo root _ t2) = t2

{-Funcion que retorna el numero de nodos de un ArbolAVL-}
numNodos::ArbolAVL t -> Int
numNodos Nulo = 0
numNodos (Nodo root t1 t2)
          | esNulo(t1) && esNulo(t2) = 1
          | esNulo(t1) = numNodos(t2) + 1
          | esNulo(t2) = numNodos(t1) + 1
          | not(esNulo(t1) && esNulo(t2)) = numNodos(t1) + numNodos(t2) + 1

{-Funcion que verifica si un nodo es Hoja-}
esHoja::ArbolAVL t -> Bool
esHoja Nulo = True
esHoja (Nodo root t1 t2) = esNulo(t1) && esNulo(t2)

{-Funcion que busca un elemento en el arbol-}
buscarAVL::(Ord t) => ArbolAVL t -> t -> ArbolAVL t
buscarAVL Nulo x = Nulo
buscarAVL (Nodo root t1 t2) x
            | x == root = (Nodo root t1 t2)
            | x  < root = buscarAVL t1 x
            | x  > root = buscarAVL t2 x


altura::(Ord t) => ArbolAVL t -> Int
altura Nulo = 0
altura x
        | esHoja(x) = 0
        | not (esNulo(izq(x))) = altura(izq(x)) + 1
        | not (esNulo(der(x))) = altura(der(x)) + 1


{-Funcion que retorna la altura de un arbolAVL-}
alturaAVL::(Ord t) => ArbolAVL t -> Int
alturaAVL Nulo = 0
alturaAVL t
            | esHoja(t)            = 0
            | alturaIzq >= alturaDer  = alturaIzq
            | alturaIzq < alturaDer   = alturaDer
            where
                alturaIzq = altura(izq(t)) + 1
                alturaDer = altura(der(t)) + 1



{- Funcion que inserta un elemento al arbolAVl -}
insertarAVL::(Ord t) => ArbolAVL t -> t -> ArbolAVL t
insertarAVL Nulo e = crearAVL (Nodo e Nulo Nulo)
insertarAVL (Nodo root t1 t2) e
              | e == root = crearAVL (Nodo root t1 t2)
              | e  > root = crearAVL (Nodo root t1 (insertarAVL(t2) e))
              | e  < root = crearAVL (Nodo root (insertarAVL(t1) e) t2)

{-Funcion que inserta un sub arbol avl-}
insertarSubTreeAVL::(Ord t) => ArbolAVL t -> ArbolAVL t-> ArbolAVL t
insertarSubTreeAVL Nulo Nulo = Nulo
insertarSubTreeAVL Nulo (Nodo y yl yr) = (Nodo y yl yr)
insertarSubTreeAVL (Nodo x xl xr) Nulo = (Nodo x xl xr)
insertarSubTreeAVL (Nodo x xl xr) (Nodo y yl yr)
            | y == x = (Nodo x xl xr)
            | y  > x = (Nodo x xl (insertarSubTreeAVL xr (Nodo y yl yr)))
            | y  < x = (Nodo x (insertarSubTreeAVL xl (Nodo y yl yr)) xr )

{- Funcion que elimina un elemento del arbolAVl -}
eliminarAVL::(Ord t) => ArbolAVL t -> t -> ArbolAVL t
eliminarAVL Nulo e = Nulo
eliminarAVL (Nodo root t1 t2) e
              | e == root = destroy(Nodo root t1 t2)
              | e  > root = (Nodo root t1 (eliminarAVL t2 e))
              | e  < root = (Nodo root (eliminarAVL t1 e) t2)

              where
                destroy (Nodo x Nulo Nulo) = Nulo
                destroy (Nodo x Nulo r) = r
                destroy (Nodo x l Nulo) = l
                destroy (Nodo x l r) = (Nodo (raiz l) (izq l) (insertarSubTreeAVL r (der l)))
