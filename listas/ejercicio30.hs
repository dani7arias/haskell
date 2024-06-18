-- Extraer una sección de una lista, desde una posición dada hasta otra
-- Extraer (posinicial, posfinal, lista)
-- El llamado a la función extraer ( 1, 3 , [1,2,4,5,6,3,6,4,2] ) devuelve [1,2,4].
-- El llamado a la función extraer ( 3, 5 , [1,2,4,5,6,3,6,4,2] ) devuelve [4,5,6].
-- Extraer ( 3, 3, [ [1,2], [ ], [4], [2,3,4], [ ] ] ) devuelve [ [ 4 ] ] .
-- Extraer ( 3 ,20, [ (1,2), (3,4), (7,9) , (71,2), (7,2) ] ) devuelve [(7,9) , (71,2), (7,2)]
                            
extraer:: Int -> Int -> [a] -> [a]
extraer _   _   []  =   []
extraer x   y   (cab:col) | (x > 1) && (x < y) = extraer (x-1) (y-1) col
                          | (x == 1) && (y > x) = cab:(extraer x (y-1) col)
                          | (x == 1) && (y == 1) = [cab]
                          | (x == y) = extraer (x-1) (y-1) col 

main:: IO()
main = do
    print $ extraer 1 3 [1,2,4,5,6,3,6,4,2]
    print $ extraer 3 5 [1,2,4,5,6,3,6,4,2]
    print $ extraer 3 3 [[1,2],[],[4],[2,3,4],[]]
    print $ extraer 3 20 [(1,2),(3,4),(7,9),(71,2),(7,2)]