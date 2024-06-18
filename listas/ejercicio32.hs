-- Dada una lista, armar una lista de pares tomando en orden de a dos elementos así:
-- armar [1,2,3,4,5,6,7,8]
-- retorna [(1,2), (3,4), (5,6), (7,8)]
-- Si el número de elementos de la lista inicial es impar, debe ignorar el último elemento
-- armar [1,2,3,4,5,6,7,8,9]
-- retorna [(1,2), (3,4), (5,6), (7,8)]

type Pares = (Int, Int)
type ListaDePares = [Pares]
type Lista = [Int]

armar:: Lista -> ListaDePares
armar   []          =   []
armar   [x]         =   []
armar   (a:(b:col)) =   (a, b):(armar col)

main:: IO()
main = do
    print $ armar [1,2,3,4,5,6,7,8,9]
    print $ armar [1,2,3,4,5,6,7,8]