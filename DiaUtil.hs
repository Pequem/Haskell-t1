module DiaUtil(
    diaUtil
)where
getDiasMes :: Bool -> Int -> Int
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

verificaDiaUtil :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
verificaDiaUtil [] mes dia = []
verificaDiaUtil ((_mes,_dia):dias) mes dia =    if _mes == mes then
                                                    if _dia == dia then
                                                        (mes,dia):dias
                                                    else
                                                        verificaDiaUtil dias mes dia
                                                else
                                                    verificaDiaUtil dias mes dia


diaUtil :: (Bool,[(Int, Int)]) -> Int -> Int -> Bool
diaUtil (bisexto, dias) mes dia = if (mes < 1) || (mes > 12) then False else (dia > 0) && (dia <= getDiasMes bisexto mes) && (length (verificaDiaUtil dias mes dia) == 0)