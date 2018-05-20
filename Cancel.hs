module Cancel(
    cancel
) where

cancelHora:: [(Int,Int)] -> Int -> [(Int,Int)]
cancelHora [] hora = []
cancelHora ((_hora,_duracao):horas) hora | _hora == hora = horas
                                         | _hora > hora = (_hora,_duracao):horas
                                         | True = (_hora,_duracao):(cancelHora horas hora)

cancelDia:: [(Int, [(Int,Int)])] -> Int -> Int -> [(Int, [(Int, Int)])]
cancelDia [] dia hora = []
cancelDia ((_dia,_horas):dias) dia hora | _dia == dia = if (cancelHora _horas hora) == [] then dias else (_dia, (cancelHora _horas hora)):dias
                                        | _dia > dia = (_dia, _horas):dias
                                        | True = (_dia, _horas):(cancelDia dias dia hora)

cancel:: [(Int, [(Int,[(Int,Int)])])] -> Int -> Int -> Int -> [(Int, [(Int, [(Int,Int)])])]
cancel [] mes dia hora = []
cancel ((_mes,_dias):agenda) mes dia hora | mes == _mes = if (cancelDia _dias dia hora) == [] then agenda else (_mes, (cancelDia _dias dia hora)):agenda
                                          | _mes > mes = (_mes, _dias):agenda
                                          | True = (_mes, _dias):(cancel agenda mes dia hora)