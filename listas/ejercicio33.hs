-- Eliminar los elementos repetidos de una lista eliminarrepetidos(lista)
-- Ejemplo: eliminarrepetidos [1,2,1,4,1,2,1,7] retorna [4,2,1,7]

type Lista = [Int]

contarVeces::  Int -> Lista -> Int
contarVeces _   []  =   0
contarVeces n   (cab:col)   |   (cab == n) = 1 + contarVeces n col
                            |   otherwise = contarVeces n col

eliminarRepetidos:: Lista -> Lista
eliminarRepetidos   []          =   []
eliminarRepetidos   (cab:col)   |   ((contarVeces cab (cab:col)) > 1) = eliminarRepetidos col
                                |   otherwise = cab:(eliminarRepetidos col)
                        
main:: IO()
main = do
    print $ eliminarRepetidos [1,2,1,4,1,2,1,7]