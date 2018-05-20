module Insert(
    insert,
    insereBreve,
    insereMenor,
    insereMaior
) where

import DiaUtil
import Disponivel

_insertHora :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
_insertHora [] hora duracao = [(hora,duracao)]
_insertHora ((_hora,_duracao):horas) hora duracao | _hora > hora = (hora,duracao):(_hora,_duracao):horas
                                                  | True = (_hora,_duracao):_insertHora horas hora duracao

_insertDia :: [(Int,[(Int,Int)])] -> [(Int, Int)] -> Int -> Int ->Int  -> [(Int,[(Int,Int)])]
_insertDia [] horasA dia hora duracao = [(dia,[(hora, duracao)])]
_insertDia ((_dia, horas):dias) horasA dia hora duracao | dia == _dia = (_dia,(_insertHora horas hora duracao)):dias 
                                                        | _dia > dia = (dia, (_insertHora [] hora duracao)):(_dia, horas):dias
                                                        | True = (_dia,horas):(_insertDia dias horas dia hora duracao)

_insert :: [(Int,[(Int, [(Int, Int)])])] -> [(Int, [(Int,Int)])] -> Int -> Int -> Int -> Int -> [(Int,[(Int, [(Int, Int)])])]
_insert [] diasA mes dia hora duracao = [(mes,[(dia,[(hora,duracao)])])]
_insert ((_mes,dias):agenda) diasA mes dia hora duracao | mes == _mes = (mes, (_insertDia dias [] dia hora duracao)):agenda
                                                        | _mes > mes = (mes, _insertDia [] [] dia hora duracao):(_mes,dias):agenda
                                                        | True = (_mes,dias):_insert agenda diasA mes dia hora duracao

insert :: [(Int,[(Int, [(Int, Int)])])] -> (Bool, [(Int, Int)]) -> Int -> Int -> Int -> Int -> [(Int,[(Int, [(Int, Int)])])]
insert agenda infoCal mes dia hora duracao = if (hora + duracao) > 18 then
                                                insert agenda infoCal mes dia hora (duracao - ((hora+duracao)-18))
                                                else
                                                    if (disponivel agenda infoCal mes dia hora duracao) then
                                                        _insert agenda [] mes dia hora duracao
                                                    else
                                                        agenda

_insereBreve :: [(Int, [(Int, [(Int, Int)])])] -> (Bool, [(Int,Int)]) -> Int -> Int -> Int -> [Int] -> [(Int, [(Int , [(Int,Int)])])]
_insereBreve agenda infoCal mes dia duracao [] = agenda
_insereBreve agenda infoCal mes dia duracao horarios = if novaAgenda == agenda then
                                                            _insereBreve agenda infoCal mes dia duracao (drop 1 horarios)
                                                        else
                                                            novaAgenda
                                                        where
                                                            novaAgenda = insert agenda infoCal mes dia (head horarios) duracao

insereBreve :: [(Int, [(Int, [(Int, Int)])])] -> (Bool, [(Int,Int)]) -> Int -> Int -> Int -> [(Int, [(Int , [(Int,Int)])])]
insereBreve agenda infoCal mes dia duracao = if (_insereBreve agenda infoCal mes dia duracao horarios) /= agenda then
                                                    _insereBreve agenda infoCal mes dia duracao horarios
                                                else
                                                    insereBreve agenda infoCal mes (dia+1) duracao
                                                where
                                                    horarios = horariosDisponiveisDoDia agenda infoCal mes dia

_insereMenorDia :: [(Int, [(Int, [(Int, Int)])])] -> (Bool, [(Int, Int)]) -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
_insereMenorDia _ _ _ _ _ 25 (_hora, _intervalo) = (_hora, _intervalo)
_insereMenorDia agenda infoCal mes duracao aDia aHora (_hora, _intervalo) = if disponivel agenda infoCal mes aDia aHora duracao then
                                                                                if intervalo < _intervalo then
                                                                                    _insereMenorDia agenda infoCal mes duracao aDia (aHora+intervaloOffset) (aHora, intervalo)
                                                                                else
                                                                                    _insereMenorDia agenda infoCal mes duracao aDia (aHora+intervaloOffset) (_hora, _intervalo)
                                                                            else
                                                                                _insereMenorDia agenda infoCal mes duracao aDia (aHora+1) (_hora, _intervalo)
                                                                            where
                                                                                intervalo = tamanhoIntervalo agenda infoCal mes aDia aHora
                                                                                intervaloOffset = if ((aHora + intervalo) > 12) && (aHora < 12) then intervalo + 2 else intervalo

_insereMenor :: [(Int, [(Int, [(Int,Int)])])] -> (Bool, [(Int, Int)]) -> Int -> Int -> Int -> (Int, Int, Int) -> (Int, Int, Int)
_insereMenor agenda infoCal mes duracao 32 (_dia, _hora, _intervalo) = (_dia, _hora, _intervalo)
_insereMenor agenda infoCal mes duracao aDia (_dia, _hora, _intervalo) = if (nIntervalo < _intervalo) then
                                                            _insereMenor agenda infoCal mes duracao (aDia+1) (aDia, nHora, nIntervalo)
                                                        else
                                                            _insereMenor agenda infoCal mes duracao (aDia+1) (_dia, _hora, _intervalo)
                                                        where
                                                            (nHora, nIntervalo) = _insereMenorDia agenda infoCal mes duracao aDia 0 (0,100)

insereMenor :: [(Int, [(Int, [(Int,Int)])])] -> (Bool, [(Int, Int)]) -> Int -> Int -> [(Int, [(Int, [(Int, Int)])])]
insereMenor agenda infoCal mes duracao = if _intervalo < 100 then
                                            insert agenda infoCal mes _dia _hora duracao
                                        else
                                            agenda
                                        where
                                            (_dia, _hora, _intervalo) = _insereMenor agenda infoCal mes duracao 0 (1,8,100)

_insereMaiorDia :: [(Int, [(Int, [(Int, Int)])])] -> (Bool, [(Int, Int)]) -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
_insereMaiorDia _ _ _ _ _ 25 (_hora, _intervalo) = (_hora, _intervalo)
_insereMaiorDia agenda infoCal mes duracao aDia aHora (_hora, _intervalo) = if disponivel agenda infoCal mes aDia aHora duracao then
                                                                                if intervalo > _intervalo then
                                                                                    _insereMaiorDia agenda infoCal mes duracao aDia (aHora+intervaloOffset) (aHora, intervalo)
                                                                                else
                                                                                    _insereMaiorDia agenda infoCal mes duracao aDia (aHora+intervaloOffset) (_hora, _intervalo)
                                                                            else
                                                                                _insereMaiorDia agenda infoCal mes duracao aDia (aHora+1) (_hora, _intervalo)
                                                                            where
                                                                                intervalo = tamanhoIntervalo agenda infoCal mes aDia aHora
                                                                                intervaloOffset = if ((aHora + intervalo) > 12) && (aHora < 12) then intervalo + 2 else intervalo

_insereMaior :: [(Int, [(Int, [(Int,Int)])])] -> (Bool, [(Int, Int)]) -> Int -> Int -> Int -> (Int, Int, Int) -> (Int, Int, Int)
_insereMaior agenda infoCal mes duracao 32 (_dia, _hora, _intervalo) = (_dia, _hora, _intervalo)
_insereMaior agenda infoCal mes duracao aDia (_dia, _hora, _intervalo) = if (nIntervalo > _intervalo) then
                                                                            _insereMaior agenda infoCal mes duracao (aDia+1) (aDia, nHora, nIntervalo)
                                                                        else
                                                                            _insereMaior agenda infoCal mes duracao (aDia+1) (_dia, _hora, _intervalo)
                                                                        where
                                                                            (nHora, nIntervalo) = _insereMaiorDia agenda infoCal mes duracao aDia 0 (0,0)

insereMaior :: [(Int, [(Int, [(Int,Int)])])] -> (Bool, [(Int, Int)]) -> Int -> Int -> [(Int, [(Int, [(Int, Int)])])]
insereMaior agenda infoCal mes duracao = if _intervalo > 0 then
                                            insert agenda infoCal mes _dia _hora duracao
                                        else
                                            agenda
                                        where
                                            (_dia, _hora, _intervalo) = _insereMaior agenda infoCal mes duracao 0 (1,8,0)

