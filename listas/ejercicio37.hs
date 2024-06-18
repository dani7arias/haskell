-- Analice el siguiente programa y practique diversos llamados a las funciones allí definidas
-- module Cambiodemoneda where

type Precio= (Moneda,Float)
data Moneda = Euro | Peseta | Lira | Marco | Franco | Florin | Escudo | Dolar | Yen | Libra | Peso | Bolivar | Sol deriving Show

unEuro:: Moneda -> Float
unEuro Euro =  1.0
unEuro Peseta = 166.386
unEuro Lira = 35.2500
unEuro Marco = 1.9606
unEuro Franco = 0.9547
unEuro Florin = 1.9351
unEuro Escudo = 110.8518
unEuro Dolar = 1.0739
unEuro Yen = 169.3082
unEuro Libra = 0.8448
unEuro Peso = 4438.6145
unEuro Bolivar = 3905951.01

unPeso:: Moneda -> Float
unPeso  Euro    =   0.00023
unPeso  Dolar   =   0.00024
unPeso  Bolivar =   0.0088
unPeso  Peso    =   1.0
unPeso  Sol     =   0.00092

cambio:: Precio -> Moneda -> Precio
cambio (m,x) nueva = (nueva, (unPeso nueva) * (x / (unPeso m)))

suma:: Moneda -> Precio -> Precio -> Precio
suma    m   p1  p2  =   let
                            (a,b) = cambio p1 m
                            (c,d) = cambio p2 m
                        in
                            (m,b+d)
main:: IO()
main = do
    print $ unPeso Euro
    print $ unPeso Dolar
    print $ unPeso Bolivar
    print $ cambio (Peso, 5000) Bolivar
    print $ cambio (Euro, unPeso Euro) Peso
    print $ suma Peso (Euro, 100) (Bolivar, 5000)
    print $ suma Dolar (Euro, 10) (Dolar, 10)