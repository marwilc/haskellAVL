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
alturaAVL root
            | esHoja(root)            = 0
            | alturaIzq >= alturaDer  = alturaIzq
            | alturaIzq < alturaDer   = alturaDer
            where
                alturaIzq = altura(izq(root)) + 1
                alturaDer = altura(der(root)) + 1



{- Funcion que iserta un arbolAVl
insertarAVL::ArbolAVL t -> t -> ArbolAVL t
insertarAVL Nulo e = Nodo e Nulo Nulo
insertarAVL (Nodo root t1 t2) e
          | root == e = Nodo (root t1 t2)
          | root  < e = Nodo (root t1 crearAVL(t2 e))
          | root  > e = Nodo (root crearAVL(t1 e) t2)

-}
