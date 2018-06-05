module Agenda.Core (
    cancel,
    disponivel,
    horariosDisponiveisDoDia,
    insert,
    insereBreve,
    insereMaior,
    insereMenor,
    reagendar
)where
import Agenda.Tipos


cancelHora:: AgendaHoras -> Hora -> AgendaHoras
cancelHora [] hora = []
cancelHora ((_hora,_duracao):horas) hora 
    | _hora == hora = horas
    | _hora > hora = (_hora,_duracao):horas
    | True = (_hora,_duracao):(cancelHora horas hora)

cancelDia:: AgendaDias -> Dia -> Hora -> AgendaDias
cancelDia [] dia hora = []
cancelDia ((_dia,_horas):dias) dia hora 
    | _dia == dia =
        if (cancelHora _horas hora) == [] 
            then dias 
            else (_dia, (cancelHora _horas hora)):dias
    | _dia > dia = (_dia, _horas):dias
    | True = (_dia, _horas):(cancelDia dias dia hora)

cancel:: Agenda -> Mes -> Dia -> Hora -> Agenda
cancel [] mes dia hora = []
cancel ((_mes,_dias):agenda) mes dia hora
    | mes == _mes = 
        if (cancelDia _dias dia hora) == [] 
            then agenda 
            else (_mes, (cancelDia _dias dia hora)):agenda
    | _mes > mes = (_mes, _dias):agenda
    | True = (_mes, _dias):(cancel agenda mes dia hora)



getDiasMes :: IsBisexto -> Mes -> NumeroDias
getDiasMes bisexto mes
    | mes == 1 = 31
    | mes == 2 = 
        if bisexto 
            then 29 
            else 28
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



                
verificaDiaUtil :: AgendaHoras -> Mes -> Dia -> AgendaHoras
verificaDiaUtil [] mes dia = []
verificaDiaUtil ((_mes,_dia):dias) mes dia =
    if _mes == mes 
        then if _dia == dia
            then (mes,dia):dias
            else verificaDiaUtil dias mes dia
        else verificaDiaUtil dias mes dia


diaUtil :: Calendario -> Mes -> Dia -> IsBisexto
diaUtil (bisexto, dias) mes dia =
    if (mes < 1) || (mes > 12)
        then False
        else (dia > 0) && (dia <= getDiasMes bisexto mes) && (length (verificaDiaUtil dias mes dia) == 0)

disponivelHora :: AgendaHoras -> HoraEDuracaoAnterior -> Hora -> Duracao -> IsHoraAjustada -> AgendaHoras
disponivelHora [] (-1, -1) hora duracao horaAjustada = 
    if (18-hora-duracao) > 0
        then [] 
        else [(hora, duracao)]
disponivelHora [] (_ha,_da) hora duracao horaAjustada = 
    if (_da+_ha) > hora
        then [(_ha, _da)]
        else if (24-hora-duracao) > 0
            then [] 
            else [(_ha, _da)]
disponivelHora ((_hora,_duracao):horas) (_ha,_da) hora duracao horaAjustada =
    if ((_hora+_duracao) > 12) && (_hora < 12) && (not (horaAjustada))
        then disponivelHora ((_hora, _duracao+2):horas) (_ha, _da) hora duracao True 
        else if hora == _hora
            then (_hora,_duracao):horas
            else if _hora > hora
                then if (_da+_ha) > hora
                    then (_hora, _duracao):horas
                    else if (hora+duracao) > _hora
                        then (_hora, _duracao):horas
                        else []
                else disponivelHora horas (_hora,_duracao) hora duracao False

disponivelDias :: AgendaDias -> Dia -> Hora -> Duracao -> AgendaHoras
disponivelDias [] dia hora duracao = []
disponivelDias ((_dia, horas):dias) dia hora duracao =
    if dia == _dia
        then disponivelHora horas (-1,-1) hora duracao False
        else disponivelDias dias dia hora duracao

disponivelMes :: Agenda -> Mes -> Dia -> Hora -> Duracao -> AgendaHoras
disponivelMes [] mes dia hora duracao = []
disponivelMes ((_mes,dias):agenda) mes dia hora duracao =
    if mes == _mes 
        then disponivelDias dias dia hora duracao 
        else disponivelMes agenda mes dia hora duracao

disponivelExceptions :: Mes -> Dia -> Hora -> Duracao -> IsValido
disponivelExceptions mes dia hora duracao =
    (((hora >= 8) && (hora < 12)) || ((hora >= 14) && ((hora+duracao) <= 18))) && (duracao > 0)

disponivel :: Agenda -> Calendario -> Mes -> Dia -> Hora -> Duracao -> IsDisponivel
disponivel agenda infoCal mes dia hora duracao =
    (disponivelExceptions mes dia hora duracao) &&  
    ((length (disponivelMes agenda mes dia hora duracao)) == 0) && 
    (diaUtil infoCal mes dia)

horariosDisponiveisDoDia :: Agenda -> Calendario -> Mes -> Dia -> Horarios
horariosDisponiveisDoDia agenda infoCal mes dia =
    [x | x <- [0..24], disponivel agenda infoCal mes dia x 1]

_tamanhoIntervalo :: Agenda -> Calendario -> Mes -> Dia -> Hora -> Tamanho -> Tamanho
_tamanhoIntervalo agenda infoCal mes dia hora tamanho =
    if (hora == 12) 
        then _tamanhoIntervalo agenda infoCal mes dia (hora+2) tamanho 
        else if disponivel agenda infoCal mes dia hora 1 
            then _tamanhoIntervalo agenda infoCal mes dia (hora+1) (tamanho+1) 
            else tamanho

tamanhoIntervalo :: Agenda -> Calendario -> Mes -> Dia -> Hora -> Tamanho
tamanhoIntervalo agenda infoCal mes dia hora =
    _tamanhoIntervalo agenda infoCal mes dia hora 0

_insertHora :: AgendaHoras -> Hora -> Duracao -> AgendaHoras
_insertHora [] hora duracao = [(hora,duracao)]
_insertHora ((_hora,_duracao):horas) hora duracao 
    | _hora > hora = (hora,duracao):(_hora,_duracao):horas
    | True = (_hora,_duracao):_insertHora horas hora duracao

_insertDia :: AgendaDias -> AgendaHoras -> Dia -> Hora -> Duracao  -> AgendaDias
_insertDia [] horasA dia hora duracao = [(dia,[(hora, duracao)])]
_insertDia ((_dia, horas):dias) horasA dia hora duracao 
    | dia == _dia = (_dia,(_insertHora horas hora duracao)):dias 
    | _dia > dia = (dia, (_insertHora [] hora duracao)):(_dia, horas):dias
    | True = (_dia,horas):(_insertDia dias horas dia hora duracao)

_insert :: Agenda -> AgendaDias -> Mes -> Dia -> Hora -> Duracao -> Agenda
_insert [] diasA mes dia hora duracao = [(mes,[(dia,[(hora,duracao)])])]
_insert ((_mes,dias):agenda) diasA mes dia hora duracao 
    | mes == _mes = (mes, (_insertDia dias [] dia hora duracao)):agenda
    | _mes > mes = (mes, _insertDia [] [] dia hora duracao):(_mes,dias):agenda
    | True = (_mes,dias):_insert agenda diasA mes dia hora duracao

insert :: Agenda -> Calendario -> Mes -> Dia -> Hora -> Duracao -> Agenda
insert agenda infoCal mes dia hora duracao = 
    if (hora + duracao) > 18 
        then insert agenda infoCal mes dia hora (duracao - ((hora+duracao)-18)) 
        else if (disponivel agenda infoCal mes dia hora duracao) 
            then _insert agenda [] mes dia hora duracao 
            else agenda

_insereBreve :: Agenda -> Calendario -> Mes -> Dia -> Duracao -> Horarios -> Agenda
_insereBreve agenda infoCal mes dia duracao [] = agenda
_insereBreve agenda infoCal mes dia duracao horarios = 
    if novaAgenda == agenda 
        then _insereBreve agenda infoCal mes dia duracao (drop 1 horarios) 
        else novaAgenda
    where
        novaAgenda = insert agenda infoCal mes dia (head horarios) duracao

insereBreve :: Agenda -> Calendario -> Mes -> Dia -> Duracao -> Agenda
insereBreve agenda infoCal mes dia duracao = 
    if (_insereBreve agenda infoCal mes dia duracao horarios) /= agenda 
        then _insereBreve agenda infoCal mes dia duracao horarios 
        else insereBreve agenda infoCal mes (dia+1) duracao
    where
        horarios = horariosDisponiveisDoDia agenda infoCal mes dia

_insereMenorDia :: Agenda -> Calendario -> Mes -> Duracao -> Dia -> Hora -> PontoDeInsercaoHora ->
    PontoDeInsercaoHora
_insereMenorDia _ _ _ _ _ 25 (_hora, _intervalo) = (_hora, _intervalo)
_insereMenorDia agenda infoCal mes duracao aDia aHora (_hora, _intervalo) = 
    if disponivel agenda infoCal mes aDia aHora duracao 
        then if intervalo < _intervalo 
            then _insereMenorDia agenda infoCal mes duracao aDia (aHora+intervaloOffset) (aHora, intervalo) 
            else _insereMenorDia agenda infoCal mes duracao aDia (aHora+intervaloOffset) (_hora, _intervalo) 
        else _insereMenorDia agenda infoCal mes duracao aDia (aHora+1) (_hora, _intervalo)
    where
        intervalo = tamanhoIntervalo agenda infoCal mes aDia aHora
        intervaloOffset = 
            if ((aHora + intervalo) > 12) && (aHora < 12)
                then intervalo + 2
                else intervalo

_insereMenor :: Agenda -> Calendario -> Mes -> Duracao -> Dia -> PontoDeInsercaoDiaHora 
    -> PontoDeInsercaoDiaHora
_insereMenor agenda infoCal mes duracao 32 (_dia, _hora, _intervalo) = (_dia, _hora, _intervalo)
_insereMenor agenda infoCal mes duracao aDia (_dia, _hora, _intervalo) = 
    if (nIntervalo < _intervalo) 
        then _insereMenor agenda infoCal mes duracao (aDia+1) (aDia, nHora, nIntervalo) 
        else _insereMenor agenda infoCal mes duracao (aDia+1) (_dia, _hora, _intervalo)
    where
        (nHora, nIntervalo) = _insereMenorDia agenda infoCal mes duracao aDia 0 (0,100)

insereMenor :: Agenda -> Calendario -> Mes -> Duracao -> Agenda
insereMenor agenda infoCal mes duracao = 
    if _intervalo < 100 
        then insert agenda infoCal mes _dia _hora duracao 
        else agenda
    where
        (_dia, _hora, _intervalo) = _insereMenor agenda infoCal mes duracao 0 (1,8,100)

_insereMaiorDia :: Agenda -> Calendario -> Mes -> Duracao -> Dia -> Hora 
    -> PontoDeInsercaoHora -> PontoDeInsercaoHora
_insereMaiorDia _ _ _ _ _ 25 (_hora, _intervalo) = (_hora, _intervalo)
_insereMaiorDia agenda infoCal mes duracao aDia aHora (_hora, _intervalo) = 
    if disponivel agenda infoCal mes aDia aHora duracao 
        then if intervalo > _intervalo 
            then _insereMaiorDia agenda infoCal mes duracao aDia (aHora+intervaloOffset) (aHora, intervalo) 
            else _insereMaiorDia agenda infoCal mes duracao aDia (aHora+intervaloOffset) (_hora, _intervalo)
        else _insereMaiorDia agenda infoCal mes duracao aDia (aHora+1) (_hora, _intervalo)
    where
        intervalo = tamanhoIntervalo agenda infoCal mes aDia aHora
        intervaloOffset = 
            if ((aHora + intervalo) > 12) && (aHora < 12) 
                then intervalo + 2 
                else intervalo

_insereMaior :: Agenda -> Calendario -> Mes -> Duracao -> Dia -> PontoDeInsercaoDiaHora 
    -> PontoDeInsercaoDiaHora
_insereMaior agenda infoCal mes duracao 32 (_dia, _hora, _intervalo) = (_dia, _hora, _intervalo)
_insereMaior agenda infoCal mes duracao aDia (_dia, _hora, _intervalo) = 
    if (nIntervalo > _intervalo) 
        then _insereMaior agenda infoCal mes duracao (aDia+1) (aDia, nHora, nIntervalo) 
        else _insereMaior agenda infoCal mes duracao (aDia+1) (_dia, _hora, _intervalo)
    where
        (nHora, nIntervalo) = _insereMaiorDia agenda infoCal mes duracao aDia 0 (0,0)

insereMaior :: Agenda -> Calendario -> Mes -> Duracao -> Agenda
insereMaior agenda infoCal mes duracao = 
    if _intervalo > 0 
        then insert agenda infoCal mes _dia _hora duracao 
        else agenda
    where
        (_dia, _hora, _intervalo) = _insereMaior agenda infoCal mes duracao 0 (1,8,0)

reagendar :: Agenda -> Calendario -> Mes -> Dia -> Hora -> Mes -> Dia -> Hora -> Duracao -> Agenda
reagendar [] infoCal mes dia hora nMes nDia nHora nDuracao = []
reagendar agenda infoCal mes dia hora nMes nDia nHora nDuracao = 
    if disponivel agenda infoCal nMes nDia nHora nDuracao 
        then insert (cancel agenda mes dia hora) infoCal nMes nDia nHora nDuracao 
        else agenda