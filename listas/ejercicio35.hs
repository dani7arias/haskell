type Complejo = (Float, Float)
type Solucion = (Complejo, Complejo)

r:: (Float,Float,Float) ->  Solucion
r   (a,b,c) =   (x1, x2)
            where
                d = (b * b) - (4*a*c)
                y = if d < 0 then
                        sqrt(-d)
                    else
                        sqrt(d)
                (x1,x2) = if d < 0 then
                            (( y/(2*a), -b/(2*a)), (-y/(2*a),-b/(2*a)))
                        else
                            ((0,(-b+y)/(2*a)),(0,(-b-y)/(2*a)))

main:: IO()
main = do
    print $ r(3,4,10)