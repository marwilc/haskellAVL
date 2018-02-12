{- Marwil Campos
-- Jhoan Moreno
-- 11/02/2018 -}

{- Modulo arbol AVL mas sus funciones predeterminadas-}
module ArbolAVL(ArbolAVL(Nulo, Nodo), nulo, esNulo{-, crearAVL, raiz, izq, der, numNodos, esHoja, insertarAVL, eliminarAVL, buscarAVL, alturaAVL, balancear-})
where -- en el modulo se especifican dos estructuras una esructura Nulo (Arbol Nulo) y un Nodo con dos sub arboles.
data ArbolAVL t = Nulo | Nodo t (ArbolAVL t) (ArbolAVL t) deriving (Show)

{- Crea un Arbol Nulo -}
nulo::ArbolAVL t -> ArbolAVL t
nulo Nulo = Nulo

{- Verifica si es un arbol Nulo-}
esNulo::ArbolAVL t -> Bool
esNulo Nulo = True
esNulo _ = False
