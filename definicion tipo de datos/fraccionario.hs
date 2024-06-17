type Fraccion=(Int,Int)
data TipoFraccion = Propia|Impropia|Unidad deriving Show


sumar::Fraccion->Fraccion->Fraccion
sumar (a,b) (c,d) = ((a*d)+(b*c) ,b*d )

hallarTipo::Fraccion-> TipoFraccion
hallarTipo   (n,d)  |n==d   = Unidad
                    |n<d    = Propia
                    |n>d    = Impropia

main:: IO()
main = do
    print $ hallarTipo (1,3)