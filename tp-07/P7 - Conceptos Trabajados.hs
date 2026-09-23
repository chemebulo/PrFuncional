--------------------------------------------------------------------------------------------------------

--                      TRABAJO PRÁCTICO N°7: Inducción y Recursión Estructural I                     --
--                                         Conceptos Trabajados                                       --

--------------------------------------------------------------------------------------------------------

> Reglas para definir conjuntos inductivos:
    > Reglas base:
        > Una o varias afirmaciones directas que determinan qué elemento es parte del conjunto.
            Ejemplo:

            Habitacion a está en el conjunto Dungeon a.
        

    > Reglas inductivas:
        > Una o varias implicaciones (productivas) que determinan qué elemento es parte del conjunto.
            Ejemplo:

            Si m está en el conjunto Maybe a, d está en el conjunto Dungeon a,
            entonces Pasaje m d está en el conjunto Dungeon a.


> Conceptos de funciones recursivas entre Tipos Algebraicos:

    > Cantidad de elementos que cumplen:

        cantidadDeEn :: Eq a => a -> Dungeon a -> Int
        cantidadDeEn x' (Habitacion x)        = unoSi (x == x')
        cantidadDeEn x' (Recto m d)           = unoSi (estaVacioM m) + cantidadDeEn x' d
        cantidadDeEn x' (Bifurcacion m d1 d2) = unoSi (estaVacioM m) + cantidadDeEn x' d1 + cantidadDeEn x' d2

        estaVacioM :: Maybe a -> Bool
        estaVacioM Nothing = True
        estaVacioM _       = False 

        unoSi :: Bool -> Int
        unoSi True  = 1
        unoSi False = 0

    > Juntar dos elementos en uno:
    
        integrantesAPlanilla :: Equipo -> Planilla
        integrantesAPlanilla (Empleado n)        = Registro n Fin 
        integrantesAPlanilla (Encargado n e1 e2) = Registro n (juntarPlanillas (integrantesAPlanilla e1)
                                                                               (integrantesAPlanilla e2))

        juntarPlanillas :: Planilla -> Planilla -> Planilla
        juntarPlanillas Fin            p' = p'
        juntarPlanillas (Registro n p) p' = Registro n (juntarPlanillas p p')

    > Existencia de un elemento:

        esta :: Nombre -> Planilla -> Bool
        esta n' Fin            = False
        esta n' (Registro n p) = n' == n || esta n' p

    > Longitud de un elemento:

        largoDePlanilla :: Planilla -> Int
        largoDePlanilla Fin            = 0
        largoDePlanilla (Registro _ p) = 1 + largoDePlanilla p


> Aclaración sobre la condición adicional en las reglas de inductivas:

    > Que se pida el menor de todos los conjuntos es porque, según la teoría, sería totalmente válido sin la regla adicional
      que sea válido un conjunto que tenga el conjunto inductivo buscado y además otros más dentro del mismo, porque
      cumpliría las reglas.
    
    > La idea entonces es que SOLAMENTE tenga sentido quedarse con el que ÚNICAMENTE tenga el conjunto inductivo buscado
      y nada más: "el menor de los que cumplen".

    > Por eso esta regla provee unicidad, ya que siempre el resultado va a ser el conjunto inductivo buscado.