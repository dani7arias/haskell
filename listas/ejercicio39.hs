type Bolsa a = [(a,Int)]

vacia:: Bolsa a -> Bool -- o bien: vacia :: [(a,Int)] -> Bool
vacia   []  =   True
vacia   _   =   False

anadir:: (Eq a) => a -> Bolsa a -> Bolsa a
anadir  x   []          =   [(x,1)]
anadir  x   ((y,n):t)   =   if (x==y) then
                                (y,n+1):t
                            else
                                (y,n):(anadir x t)

anadir2:: (Eq a) => (a,Int) -> Bolsa a -> Bolsa a
anadir2  p      []          =   [p]
anadir2  (z,m)  ((y,n):t)   =   if (z==y) then 
                                    (y,n+m):t
                                else
                                    (y,n):(anadir2 (z,m) t)
unir:: (Eq a) => Bolsa a -> Bolsa a -> Bolsa a
unir    (h:t)   b   =   unir t (anadir2 h b) -- o bien anadir2 h (unir t b)
unir    []      b   =   b

main:: IO()
main = do
    ---en estos llamados la “a” representa números enteros
    print $ anadir 3 []
    print $ anadir 5 (anadir 3 [])
    print $ anadir 3 (anadir 2(anadir 3 (anadir 5 (anadir 3 []))))

    ---en estos dos llamados la “a” representa una lista de enteros
    print $ anadir [1,2] [([],1)]
    print $ anadir [1,2] (anadir [1,2] [([],1)])