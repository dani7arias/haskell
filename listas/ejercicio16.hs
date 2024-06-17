-- Hallar el promedio de una lista de números
sumarNumeros:: [Int] -> Int
sumarNumeros    []          =   0
sumarNumeros    (cab:col)   =   cab + sumarNumeros col

contarDatos:: [Int] -> Int
contarDatos []          =   0
contarDatos (cab:col)   =   1 + contarDatos col

promedio:: [Int] -> Int
promedio    []  =   0
promedio    lista   = div (sumarNumeros lista) (contarDatos lista)   

main:: IO()
main = do
    print $ promedio [3,2,1]