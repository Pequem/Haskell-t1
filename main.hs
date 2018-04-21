getDiasMes bisexto mes  | mes == 1 = 31
                        | mes == 2 = if bisexto then 29 else 28
                        | mes == 3 = 31
                        | mes == 4 = 30
                        | mes == 5 = 31
                        | mes == 6 = 30
                        | mes == 7 = 31
                        | mes == 8 = 31
                        | mes == 9 = 30
                        | mes == 10 = 31
                        | mes == 11 = 30
                        | mes == 12 = 31

verificaDiaUtil [] mes dia = []
verificaDiaUtil ((_mes,_dia):dias) mes dia =    if _mes == mes then
                                                    if _dia == dia then
                                                        (mes,dia):dias
                                                    else
                                                        verificaDiaUtil dias mes dia
                                                else
                                                    verificaDiaUtil dias mes dia

diaUtil :: (Bool,[(Int, Int)]) -> Int -> Int -> Bool
diaUtil (bisexto, dias) mes dia =   (dia <= getDiasMes bisexto mes) && (length (verificaDiaUtil dias mes dia) == 0)

disponivelHora [] (-1, -1) hora duracao = []
disponivelHora [] (_ha,_da) hora duracao =  if (_da+_ha) > hora then
                                                [(_ha, _da)]
                                            else
                                                []    
disponivelHora ((_hora,_duracao):horas) (_ha,_da) hora duracao =    if hora == _hora then
                                                                        (_hora,_duracao):horas
                                                                    else
                                                                        if _hora > hora then
                                                                            if (_da+_ha) > hora then
                                                                                (_hora, _duracao):horas
                                                                            else
                                                                                if (hora+duracao) > _hora then
                                                                                    (_hora, _duracao):horas
                                                                                else
                                                                                    []
                                                                        else
                                                                            disponivelHora horas (_hora,_duracao) hora duracao

disponivelDias [] dia hora duracao = []
disponivelDias ((_dia, horas):dias) dia hora duracao =  if dia == _dia then
                                                            disponivelHora horas (-1,-1) hora duracao
                                                        else
                                                            disponivelDias dias dia hora duracao

disponivelMes [] mes dia hora duracao = []
disponivelMes ((_mes,dias):agenda) mes dia hora duracao =   if mes == _mes then
                                                                disponivelDias dias dia hora duracao
                                                            else 
                                                                disponivelMes agenda mes dia hora duracao

disponivel :: [(Int,[(Int, [(Int, Int)])])] -> Int -> Int -> Int -> Int -> Bool
disponivel agenda mes dia hora duracao = (length (disponivelMes agenda mes dia hora duracao)) == 0

_insertHora :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
_insertHora [] hora duracao = [(hora,duracao)]
_insertHora ((_hora,_duracao):horas) hora duracao | _hora > hora = (hora,duracao):(_hora,_duracao):horas
                                                  | True = (_hora,_duracao):_insertHora horas hora duracao

_insertDia :: [(Int,[(Int,Int)])] -> [(Int, Int)] -> Int -> Int ->Int  -> [(Int,[(Int,Int)])]
_insertDia [] horasA dia hora duracao = [(dia,[(hora, duracao)])]
_insertDia ((_dia, horas):dias) horasA dia hora duracao | dia == _dia = (_dia,(_insertHora horas hora duracao)):dias 
                                                        | _dia > dia = (dia, (_insertHora horasA hora duracao)):(_dia, horas):dias
                                                        | True = (_dia,horas):(_insertDia dias horas dia hora duracao)

_insert :: [(Int,[(Int, [(Int, Int)])])] -> [(Int, [(Int,Int)])] -> Int -> Int -> Int -> Int -> [(Int,[(Int, [(Int, Int)])])]
_insert [] diasA mes dia hora duracao = [(mes,[(dia,[(hora,duracao)])])]
_insert ((_mes,dias):agenda) diasA mes dia hora duracao | mes == _mes = (mes, (_insertDia dias [] dia hora duracao)):agenda
                                                        | _mes > mes = (mes, _insertDia diasA [] dia hora duracao):(_mes,dias):agenda
                                                        | True = (_mes,dias):_insert agenda diasA mes dia hora duracao

insert :: [(Int,[(Int, [(Int, Int)])])] -> (Bool, [(Int, Int)]) -> Int -> Int -> Int -> Int -> [(Int,[(Int, [(Int, Int)])])]
insert agenda infoCal mes dia hora duracao = if (diaUtil infoCal mes dia) && (disponivel agenda mes dia hora duracao) then
                                                _insert agenda [] mes dia hora duracao
                                            else
                                                agenda
