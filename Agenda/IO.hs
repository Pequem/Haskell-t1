module Agenda.IO(
    gravaAgenda,
    lerAgenda,
    lerInfo
)where

import System.IO
import Agenda.Tipos

_nextInt :: String -> String -> String
_nextInt [] numero = numero
_nextInt (c:linha) numero = 
    if c == ' '
        then numero
        else _nextInt linha (c:numero) 

_infoadd :: DiasCalendario -> Mes -> String -> DiasCalendario
_infoadd infos mes buff = 
    if numChar == []
        then if (length buff) == 0
            then infos
            else _infoadd infos mes (drop 1 buff)
        else _infoadd ((mes, (read numChar :: Dia)):infos) mes (drop (length numChar) buff)
        where
            numChar = reverse (_nextInt buff [])
                
_lerInfo :: Handle -> Mes -> DiasCalendario -> IO DiasCalendario
_lerInfo handle mes infos =  
    do
        eof <- hIsEOF handle
        if eof
            then return (infos) 
            else do 
                buff <- hGetLine handle
                _lerInfo handle (mes+1) (_infoadd infos mes buff)

lerInfo :: IO Calendario
lerInfo = 
    do
        handle <- openFile "calendario.txt" ReadMode
        op <- hGetLine handle
        _infos <- _lerInfo handle 1 []
        hClose handle
        if op == "False" 
            then return (False, reverse (_infos))
            else return (True, reverse (_infos))

_gravaHoras :: Handle -> AgendaHoras -> IO ()
_gravaHoras handle horas = 
    do
        hPutStrLn handle (show horas)
        return ()

_gravaDias :: Handle -> AgendaDias -> IO ()
_gravaDias handle [] = 
    do 
        return ()
_gravaDias handle ((dia,horas):dias) = 
    do
        hPutStrLn handle (show dia)
        _gravaHoras handle horas
        _gravaDias handle dias
        return ()

_gravaMeses :: Handle -> Agenda -> IO ()
_gravaMeses handle [] = 
    do 
        return ()
_gravaMeses handle ((mes,dias):agenda) = 
    do
        hPutStrLn handle (show mes)
        _gravaDias handle dias
        hPutStrLn handle ""
        _gravaMeses handle agenda
        return ();

gravaAgenda :: Agenda -> IO ()
gravaAgenda agenda = 
    do
        handle <- openFile "agenda.txt" WriteMode
        _gravaMeses handle agenda
        hFlush handle
        hClose handle
                        
lerHoras :: Handle -> IO AgendaHoras
lerHoras handle = 
    do
        horas <- hGetLine handle
        return (read horas :: AgendaHoras)

lerDia :: Handle -> AgendaDias -> IO AgendaDias
lerDia handle dias = 
    do
        final <- hIsEOF handle
        if final
        then return (reverse dias)
        else do
                dia <- hGetLine handle
                if dia == "" 
                    then do return (reverse dias)
                    else do
                            horas <- lerHoras  handle
                            lerDia handle ((read dia,horas):dias)

lerMes :: Handle -> Agenda -> IO Agenda
lerMes handle meses = 
    do
        final <- hIsEOF handle
        if final
            then return (reverse meses)
            else do
                    mes <- hGetLine handle
                    if mes == "" 
                        then lerMes handle meses
                        else do
                                dias <- lerDia handle []
                                lerMes handle ((read mes,dias):(meses))

lerAgenda :: IO Agenda
lerAgenda = 
    do
        handle <- openFile "agenda.txt" ReadMode
        agenda <- lerMes handle []
        hClose handle
        return agenda