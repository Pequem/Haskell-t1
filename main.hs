module Main(
        main
) where
import Agenda.Core
import Agenda.IO
import Agenda.Tipos
import System.IO

imprimeOpcoes :: IO ()
imprimeOpcoes = 
    do
        putStrLn "0 - Sair"
        putStrLn "1 - Recuperar agenda"
        putStrLn "2 - Verificar disponibilidade de horário"
        putStrLn "3 - Verificar disponibilidade no dia"
        putStrLn "4 - Inserir compromisso no horário"
        putStrLn "5 - Inserir compromisso mais breve"
        putStrLn "6 - Inserir compromisso no intervalo mínimo"
        putStrLn "7 - Inserir compromisso no intervalo máximo"
        putStrLn "8 - Cancelar compromisso"
        putStrLn "9 - Reagendar compromisso"
        putStrLn "10 - Gravar agenda"

trataOpcao1 :: IO Agenda
trataOpcao1 = lerAgenda

trataOpcao2 :: Agenda -> Calendario -> IO IsDisponivel
trataOpcao2 agenda info =
    do
        putStr "mes:"
        hFlush stdout
        mes <- getLine
        putStr "dia:" 
        hFlush stdout
        dia <- getLine
        putStr "hora:"
        hFlush stdout
        hora <- getLine
        putStr "duracao:"
        hFlush stdout
        duracao <- getLine
        if not ((mes == "") || (dia == "") || (hora == "") || (duracao == ""))
            then return (disponivel agenda info (read mes) (read dia) (read hora) (read duracao))
            else trataOpcao2 agenda info

trataOpcao3 :: Agenda -> Calendario -> IO Horarios
trataOpcao3 agenda info = 
    do
        putStr "mes:"
        hFlush stdout
        mes <- getLine
        putStr "dia:" 
        hFlush stdout
        dia <- getLine
        if not ((mes == "") || (dia == "")) 
            then return (horariosDisponiveisDoDia agenda info (read mes) (read dia))
            else trataOpcao3 agenda info

trataOpcao4 :: Agenda -> Calendario -> IO Agenda
trataOpcao4 agenda info = 
    do
        putStr "mes:"
        hFlush stdout
        mes <- getLine
        putStr "dia:" 
        hFlush stdout
        dia <- getLine
        putStr "hora:"
        hFlush stdout
        hora <- getLine
        putStr "duracao:"
        hFlush stdout
        duracao <- getLine
        if not ((mes == "") || (dia == "") || (hora == "") || (duracao == "")) 
            then return (insert agenda info (read mes) (read dia) (read hora) (read duracao))
            else trataOpcao4 agenda info

trataOpcao5 :: Agenda -> Calendario -> IO Agenda
trataOpcao5 agenda info = 
    do
        putStr "mes:"
        hFlush stdout
        mes <- getLine
        putStr "dia:" 
        hFlush stdout
        dia <- getLine
        putStr "duracao:"
        hFlush stdout
        duracao <- getLine
        if not ((mes == "") || (dia == "") || (duracao == "")) 
            then return (insereBreve agenda info (read mes) (read dia) (read duracao))
            else trataOpcao5 agenda info

trataOpcao6 :: Agenda -> Calendario -> IO Agenda
trataOpcao6 agenda info = 
    do
        putStr "mes:"
        hFlush stdout
        mes <- getLine
        putStr "duracao:"
        hFlush stdout
        duracao <- getLine
        if not ((mes == "") || (duracao == "")) 
            then return (insereMenor agenda info (read mes) (read duracao))
            else trataOpcao6 agenda info
                            
trataOpcao7 :: Agenda -> Calendario -> IO Agenda
trataOpcao7 agenda info = 
    do
        putStr "mes:"
        hFlush stdout
        mes <- getLine
        putStr "duracao:"
        hFlush stdout
        duracao <- getLine
        if not ((mes == "") || (duracao == ""))
            then return (insereMaior agenda info (read mes) (read duracao))
            else trataOpcao7 agenda info

trataOpcao8 :: Agenda -> Calendario -> IO Agenda
trataOpcao8 agenda info = 
    do
        putStr "mes:"
        hFlush stdout
        mes <- getLine
        putStr "dia:"
        hFlush stdout
        dia <- getLine
        putStr "hora:"
        hFlush stdout
        hora <- getLine
        if not ((mes == "") || (dia == "") || (hora == "")) 
            then return (cancel agenda (read mes) (read dia) (read hora))
            else trataOpcao8 agenda info

trataOpcao9 :: Agenda -> Calendario -> IO Agenda
trataOpcao9 agenda info = 
    do
        putStr "mes Antigo:"
        hFlush stdout
        ames <- getLine
        putStr "dia Antigo:" 
        hFlush stdout
        adia <- getLine
        putStr "hora Antigo:"
        hFlush stdout
        ahora <- getLine
        putStr "mes:"
        hFlush stdout
        mes <- getLine
        putStr "dia:" 
        hFlush stdout
        dia <- getLine
        putStr "hora:"
        hFlush stdout
        hora <- getLine
        putStr "duracao:"
        hFlush stdout
        duracao <- getLine
        if not ((ames == "") || (adia == "") || (ahora == "") || (mes == "") || (dia == "") 
                                                         || (hora == "") || (duracao == "")) 
            then return (reagendar agenda info (read ames) (read adia) (read ahora) (read mes) 
                                                        (read dia) (read hora) (read duracao))
            else trataOpcao9 agenda info

boolToString :: Bool -> String
boolToString bool = 
    if bool 
        then "True" 
        else "False"

trataOpcao :: Agenda -> Calendario -> String -> IO Agenda
trataOpcao  agenda info op = 
    do 
        if op == "1" 
            then do
                _agenda <- trataOpcao1
                return _agenda
            else if op == "2" 
                then do
                    dis <- trataOpcao2 agenda info
                    putStrLn (boolToString dis)
                    return agenda
                else if op == "3" 
                    then do
                        dis <- trataOpcao3 agenda info
                        putStrLn (show dis)
                        return agenda
                    else if op == "4" 
                        then do
                            _agenda <- trataOpcao4 agenda info
                            return _agenda
                        else if op == "5" 
                            then do
                                _agenda <- trataOpcao5 agenda info
                                return _agenda
                            else if op == "6" 
                                then do
                                    _agenda <- trataOpcao6 agenda info
                                    return _agenda
                                else if op == "7" 
                                    then do
                                        _agenda <- trataOpcao7 agenda info
                                        return _agenda
                                    else if op == "8" 
                                        then do
                                            _agenda <- trataOpcao8 agenda info
                                            return _agenda
                                        else if op == "9" 
                                            then do
                                                _agenda <- trataOpcao9 agenda info
                                                return _agenda
                                            else if op == "10" 
                                                then do
                                                    gravaAgenda agenda
                                                    return agenda
                                                else
                                                    return agenda
    
loop :: Agenda -> Calendario -> IO ()
loop agenda infos = 
    do
        putStrLn (show agenda)
        imprimeOpcoes
        hFlush stdout
        op <- getLine
        if op == "0" 
            then return ()
            else do                 
                agenda <- trataOpcao agenda infos op
                loop agenda infos

main :: IO ()
main = 
    do
        agenda <- lerAgenda
        infos <- lerInfo
        loop agenda infos