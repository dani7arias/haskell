-- Hacer diferentes funciones y de manera generalizada que 
-- permitan eliminar un elemento de una lista, 
-- agregar un elemento al final de una lista, 
-- agregar un elemento a una lista en una posición determinada, 
-- eliminar un elemento que se encuentra en una posición determinada dentro de la lista

eliminarElemento:: Eq a => a -> [a] -> [a]
eliminarElemento    _   []          =   []
eliminarElemento    n   (cab:col)   |   cab == n    =   col
                                    |   otherwise   =   (cab:(eliminarElemento n col))

eliminarDatoTodosRepetido::Eq a=> a -> [a] -> [a]
eliminarDatoTodosRepetido    _   []         =   []
eliminarDatoTodosRepetido    n   (cab:col)  |   cab == n    =   (eliminarDatoTodosRepetido n col)
                                            |   otherwise   =   (cab:(eliminarDatoTodosRepetido n col))

eliminarRepetidosDejarUno:: Eq a => a -> [a] -> [a]
eliminarRepetidosDejarUno   _    []         =   []
eliminarRepetidosDejarUno   n    (cab:col)  |   (cab == n) && (contador > 1) = (eliminarRepetidosDejarUno n col)
                                            |   otherwise = (cab:(eliminarRepetidosDejarUno n col))
                                            where
                                                contador = contarVecesDato cab (cab:col)

contarVecesDato:: Eq a=> a -> [a] -> Int
contarVecesDato     _   []          =   0
contarVecesDato     n   (cab:col)   |   cab == n = 1 + (contarVecesDato n col)
                                    |   otherwise = (contarVecesDato n col)

agregarFinal:: a -> [a] -> [a]
agregarFinal    n   []          =   [n]
agregarFinal    n   (cab:col)   =   (cab:(agregarFinal n col))

agregarElementoPosicion:: Int -> a -> [a] -> [a]
agregarElementoPosicion     1   x   lista       =   (x:lista)
agregarElementoPosicion     n   x   (cab:col)   =   (cab:(agregarElementoPosicion (n-1) x col))

eliminarElementoPosicion:: Int -> [a] -> [a]
eliminarElementoPosicion    1   (_:col)     =   col
eliminarElementoPosicion    n   (cab:col)   =   (cab:(eliminarElementoPosicion (n-1) col))


eliminarTodosRepetidos:: Eq a => [a] -> [a]
eliminarTodosRepetidos  []  =   []
eliminarTodosRepetidos  (cab:col)   | (contador > 1) = r
                                    | otherwise = (cab:r)
                                    where
                                        contador = contarVecesDato cab (cab:col)
                                        r = eliminarTodosRepetidos col


main:: IO()
main = do
    print $ eliminarTodosRepetidos [1,1,1,3,3,3,4,4,4,4]
    print $ eliminarRepetidosDejarUno 1 [1,1,1,2,3,4,5]