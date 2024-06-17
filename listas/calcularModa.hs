contarDatoRepetido:: Int -> [Int] -> Int
contarDatoRepetido  _   []  =   0
contarDatoRepetido  n   (cab:col)   | cab == n = 1 + contarDatoRepetido n col
                                    | otherwise = contarDatoRepetido n col

moda:: [Int] -> Int
moda    [x]  =   x
moda    lista   |   contarA > contarB = moda (a:col)
                |   contarA < contarB = moda (b:col)
                |   otherwise = moda (a:col)
                where
                    contarA = contarDatoRepetido a lista
                    contarB = contarDatoRepetido b lista
                    (a:(b:col)) = lista

main:: IO()
main = do
    print $ moda [5,3,5,3,5,7]
    print $ moda [1,2,2,2,3,6]
    print $ moda [2,2]