module Menu(
        main
) where
import Insert
import Disponivel
import Cancel
import Reagendar
import System.IO
import LerInfos
import LerAgenda

--opcaoInvalida :: [(Int, [(Int, [(Int, Int)])])] -> (Bool, [(Int,Int)]) -> IO ()
--opcaoInvalida agenda info   = do
                                --putStrLn("Opção Inválida, escolha uma opção de 0 a 10")
                                --_agenda <- agenda
                                --_
                                --trataOpcao agenda info

imprimeOpcoes = do
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

trataOpcao1 = lerAgenda

trataOpcao2 agenda info = do
                            putStrLn "mes:"
                            mes <- getLine
                            putStrLn "dia:" 
                            dia <- getLine
                            putStrLn "hora:"
                            hora <- getLine
                            putStrLn "duracao:"
                            duracao <- getLine
                            return (disponivel agenda info (read mes) (read dia) (read hora) (read duracao))

trataOpcao3 agenda info = do
                            putStrLn "mes:"
                            mes <- getLine
                            putStrLn "dia:" 
                            dia <- getLine
                            return (horariosDisponiveisDoDia agenda info (read mes) (read dia))

trataOpcao4 agenda info = do
                            putStrLn "mes:"
                            mes <- getLine
                            putStrLn "dia:" 
                            dia <- getLine
                            putStrLn "hora:"
                            hora <- getLine
                            putStrLn "duracao:"
                            duracao <- getLine
                            _agenda <- return (insert agenda info (read mes) (read dia) (read hora) (read duracao))
                            return _agenda

trataOpcao5 agenda info = do
                            putStrLn "mes:"
                            mes <- getLine
                            putStrLn "dia:" 
                            dia <- getLine
                            putStrLn "duracao:"
                            duracao <- getLine
                            agenda <- return (insereBreve agenda info (read mes) (read dia) (read duracao))
                            return agenda

trataOpcao6 agenda info = do
                            putStrLn "mes:"
                            mes <- getLine
                            putStrLn "duracao:"
                            duracao <- getLine
                            agenda <- return (insereMenor agenda info (read mes) (read duracao))
                            return agenda

trataOpcao7 agenda info = do
                            putStrLn "mes:"
                            mes <- getLine
                            putStrLn "duracao:"
                            duracao <- getLine
                            agenda <- return (insereMaior agenda info (read mes) (read duracao))
                            return agenda

trataOpcao8 agenda info = do
                            putStrLn "mes:"
                            mes <- getLine
                            putStrLn "dia:" 
                            dia <- getLine
                            putStrLn "hora:"
                            hora <- getLine
                            _agenda <- return (cancel agenda (read mes) (read dia) (read hora))
                            return _agenda

trataOpcao9 agenda info = do
                            putStrLn "mes Antigo:"
                            ames <- getLine
                            putStrLn "dia Antigo:" 
                            adia <- getLine
                            putStrLn "hora Antigo:"
                            ahora <- getLine
                            putStrLn "mes:"
                            mes <- getLine
                            putStrLn "dia:" 
                            dia <- getLine
                            putStrLn "hora:"
                            hora <- getLine
                            putStrLn "duracao:"
                            duracao <- getLine
                            agenda <- return (reagendar agenda info (read ames) (read adia) (read ahora) (read mes) (read dia) (read hora) (read duracao))
                            return agenda

boolToString bool = if bool then "True" else "False"

trataOpcao :: [(Int, [(Int, [(Int, Int)])])] -> (Bool, [(Int,Int)]) -> [Char] -> IO [(Int, [(Int, [(Int, Int)])])]
trataOpcao  agenda info op = do 
                                if op == "1" then
                                    do
                                        _agenda <- trataOpcao1
                                        return _agenda
                                    else if op == "2" then
                                        do
                                            dis <- trataOpcao2 agenda info
                                            putStrLn (boolToString dis)
                                            return agenda
                                        else if op == "3" then
                                            do
                                                dis <- trataOpcao3 agenda info
                                                putStrLn (show dis)
                                                return agenda
                                            else if op == "4" then
                                                do
                                                    _agenda <- trataOpcao4 agenda info
                                                    return _agenda
                                                else if op == "5" then
                                                    do
                                                        _agenda <- trataOpcao5 agenda info
                                                        return _agenda
                                                    else if op == "6" then
                                                        do
                                                            _agenda <- trataOpcao6 agenda info
                                                            return _agenda
                                                        else if op == "7" then
                                                            do
                                                                _agenda <- trataOpcao7 agenda info
                                                                return _agenda
                                                            else if op == "8" then
                                                                do
                                                                    _agenda <- trataOpcao8 agenda info
                                                                    return _agenda
                                                                else if op == "9" then
                                                                    do
                                                                        _agenda <- trataOpcao9 agenda info
                                                                        return _agenda
                                                                    else if op == "10" then
                                                                        return []
                                                                        else
                                                                            return []
                        
loop agenda infos = do
                    imprimeOpcoes
                    op <- getLine
                    if op == "0" then
                        return ()
                        else
                            do                 
                                agenda <- trataOpcao agenda infos op
                                putStrLn (show agenda)
                                loop agenda infos
main = do
        infos <- lerInfo
        loop [] infos
        