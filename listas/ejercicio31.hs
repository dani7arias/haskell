-- Dada una lista de listas de sustituya las listas que terminen en un elemento dado; 
-- por la lista vacía. Por ejemplo, en el llamado
-- Sustituir ( 3 , [[1,2,5],[3],[],[6,7,3,4],[4,3]] ) 
-- devuelve la lista de listas [[1,2,5],[],[],[6,7,3,4],[]]

data UltimoElemento a = Vacio | Ultimo a deriving (Show, Eq)

ultimoDato:: [a] -> UltimoElemento a
ultimoDato  []          =   Vacio
ultimoDato  [x]         =   Ultimo x
ultimoDato  (cab:col)   =   ultimoDato col

sustituir:: Eq a => a -> [[a]] -> [[a]]
sustituir   _   []  =   []
sustituir   n   (cab:col)   |   datoBuscado == datoUltimo = []:(sustituir n col)
                            |   otherwise   =  cab:(sustituir n col)
                            where
                                datoBuscado = Ultimo n
                                datoUltimo  = ultimoDato cab

main :: IO ()
main = do
    print $ sustituir 3 [[1,2,5],[3],[],[6,7,3,4],[4,3]]