-- Realizar un programa en Haskell que permita verificar si los elementos de una lista de lista, 
-- están en una lista sencilla.
-- Contenido (listadelistas, listasencilla)
-- contenido ( [ [ ], [1,7] , [1,2] ], [7,2,3] ) retorna true

verificarListas:: Eq a=> [a] -> [a] -> Bool
verificarListas     []          _       =   True
verificarListas     _           []      =   False
verificarListas     [x]         lista   =   validar x lista
verificarListas     (cab:col)   lista   |   (validar cab lista) = True
                                        |   otherwise = verificarListas col lista

validar:: Eq a=> a -> [a] -> Bool
validar     _   []          =   False
validar     n   (cab:col)   |   cab == n    =   True
                            |   otherwise   =   (validar n col)

verificarListaDeListas:: Eq a=> [[a]] -> [a] -> Bool
verificarListaDeListas  [x]         lista   =   verificarListas x lista
verificarListaDeListas  (cab:col)   lista   |   (verificarListas cab lista) = verificarListaDeListas col lista
                                            |   otherwise = False

main:: IO()
main = do
    print $ verificarListaDeListas [[],[1,7],[1,2]] [7,2,3]
