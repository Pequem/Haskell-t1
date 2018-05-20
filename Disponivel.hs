module Disponivel(
    disponivel,
    horariosDisponiveisDoDia,
    tamanhoIntervalo
) where
import DiaUtil

disponivelHora :: [(Int, Int)] -> (Int, Int) -> Int -> Int -> Bool -> [(Int,Int)]
disponivelHora [] (-1, -1) hora duracao horaAjustada = if (18-hora-duracao) > 0 then
                                                            [] 
                                                        else
                                                            [(hora, duracao)]
disponivelHora [] (_ha,_da) hora duracao horaAjustada =  if (_da+_ha) > hora then
                                                            [(_ha, _da)]
                                                        else
                                                            if (24-hora-duracao) > 0 then
                                                                [] 
                                                            else
                                                                [(_ha, _da)]
disponivelHora ((_hora,_duracao):horas) (_ha,_da) hora duracao horaAjustada =    if ((_hora+_duracao) > 12) && (_hora < 12) && (not (horaAjustada)) then
                                                                                    disponivelHora ((_hora, _duracao+2):horas) (_ha, _da) hora duracao True 
                                                                                else
                                                                                if hora == _hora then
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
                                                                                        disponivelHora horas (_hora,_duracao) hora duracao False

disponivelDias :: [(Int,[(Int, Int)])] -> Int -> Int -> Int -> [(Int,Int)]
disponivelDias [] dia hora duracao = []
disponivelDias ((_dia, horas):dias) dia hora duracao =  if dia == _dia then
                                                            disponivelHora horas (-1,-1) hora duracao False
                                                        else
                                                            disponivelDias dias dia hora duracao

disponivelMes :: [(Int,[(Int,[(Int,Int)])])] -> Int -> Int -> Int -> Int -> [(Int,Int)]
disponivelMes [] mes dia hora duracao = []
disponivelMes ((_mes,dias):agenda) mes dia hora duracao =   if mes == _mes then
                                                                disponivelDias dias dia hora duracao
                                                            else 
                                                                disponivelMes agenda mes dia hora duracao

disponivelExceptions :: Int -> Int -> Int -> Int -> Bool
disponivelExceptions mes dia hora duracao = (((hora >= 8) && (hora < 12)) || ((hora >= 14) && ((hora+duracao) <= 18))) && (duracao > 0)

disponivel :: [(Int,[(Int, [(Int, Int)])])] -> (Bool, [(Int, Int)]) -> Int -> Int -> Int -> Int -> Bool
disponivel agenda infoCal mes dia hora duracao = (disponivelExceptions mes dia hora duracao) && ((length (disponivelMes agenda mes dia hora duracao)) == 0) && (diaUtil infoCal mes dia)

horariosDisponiveisDoDia :: [(Int, [(Int, [(Int,Int)])])] -> (Bool, [(Int,Int)]) -> Int -> Int -> [Int]
horariosDisponiveisDoDia agenda infoCal mes dia = [x | x <- [0..24], disponivel agenda infoCal mes dia x 1]

_tamanhoIntervalo :: [(Int,[(Int, [(Int, Int)])])] -> (Bool, [(Int, Int)]) -> Int -> Int -> Int -> Int -> Int
_tamanhoIntervalo agenda infoCal mes dia hora tamanho = if (hora == 12) then _tamanhoIntervalo agenda infoCal mes dia (hora+2) tamanho else
                                                        if disponivel agenda infoCal mes dia hora 1 then
                                                            _tamanhoIntervalo agenda infoCal mes dia (hora+1) (tamanho+1)
                                                        else
                                                            tamanho

tamanhoIntervalo :: [(Int,[(Int, [(Int, Int)])])] -> (Bool, [(Int, Int)]) -> Int -> Int -> Int -> Int
tamanhoIntervalo agenda infoCal mes dia hora = _tamanhoIntervalo agenda infoCal mes dia hora 0