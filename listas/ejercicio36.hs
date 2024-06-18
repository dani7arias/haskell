{-
Dado el siguiente fragmento de código en haskell sobre información de una biblioteca.
type Usuario = String
type Libro = String
type Prestamos = [(Usuario,Libro,Fecha)]
type Fecha = (Int,Int,Int)

Hacer las siguientes funciones:
Una función que dado dos fechas retorne verdadero si la primera fecha es anterior a la segunda
Una función que dada una lista de tipo Prestamos, retorne el número de libros cuya fecha de préstamo es anterior a una fecha dada
-}

type Usuario = String
type Libro = String
type Prestamos = [(Usuario,Libro,Fecha)]
type Fecha = (Int,Int,Int)

fechaAnterior :: Fecha -> Fecha -> Bool
fechaAnterior (a, b, c) (x, y, z)   |   a <= x = True
                                    |   a == x && b <= y = True
                                    |   a == x && b == y && c <= z = True
                                    |   otherwise = False

buscarLibros:: Fecha -> Prestamos -> [Libro]
buscarLibros    _       []  =   []
buscarLibros    fecha1  prestamo    |   (fechaAnterior fecha2 fecha1) = libro:recursivo
                                    |   otherwise = recursivo
                                    where
                                        ((_, libro, fecha2):col) = prestamo
                                        recursivo = buscarLibros fecha1 col


main:: IO()
main = do
    print $ buscarLibros (17, 6, 2024) [("Daniel", "Calculo", (16, 6, 2024)),
                                        ("Juan", "Quimica", (15, 6, 2024)),
                                        ("Juan", "Sistemas", (17, 6, 2024)),
                                        ("Juan", "Catedra", (10, 6, 2024))]