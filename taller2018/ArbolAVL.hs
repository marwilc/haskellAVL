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

{- Funcion que iserta un arbolAVl
insertarAVL::ArbolAVL t -> t -> ArbolAVL t
insertarAVL Nulo e = Nodo e Nulo Nulo
insertarAVL (Nodo root t1 t2) e
          | root == e = Nodo (root t1 t2)
          | root  < e = Nodo (root t1 crearAVL(t2 e))
          | root  > e = Nodo (root crearAVL(t1 e) t2)

-}
