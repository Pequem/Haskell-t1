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
import GravaAgenda

--opcaoInvalida :: [(Int, [(Int, [(Int, Int)])])] -> (Bool, [(Int,Int)]) -> IO ()
--opcaoInvalida agenda info   = do
                                --putStrLn("Opção Inválida, escolha uma opção de 0 a 10")
                                --_agenda <- agenda
                                --_
                                --trataOpcao agenda info

imprimeOpcoes :: IO ()
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

trataOpcao1 :: IO [(Int,[(Int,[(Int,Int)])])]
trataOpcao1 = lerAgenda

trataOpcao2 :: [(Int,[(Int,[(Int,Int)])])] -> (Bool, [(Int,Int)]) -> IO Bool
trataOpcao2 agenda info = do
                            putStr "mes:"
                            mes <- getLine
                            putStr "dia:" 
                            dia <- getLine
                            putStr "hora:"
                            hora <- getLine
                            putStr "duracao:"
                            duracao <- getLine
                            if not ((mes == "") || (dia == "") || (hora == "") || (duracao == "")) then
                                return (disponivel agenda info (read mes) (read dia) (read hora) (read duracao))
                                else
                                    trataOpcao2 agenda info

trataOpcao3 :: [(Int,[(Int,[(Int,Int)])])] -> (Bool, [(Int,Int)]) -> IO [Int]
trataOpcao3 agenda info = do
                            putStr "mes:"
                            mes <- getLine
                            putStr "dia:" 
                            dia <- getLine
                            if not ((mes == "") || (dia == "")) then
                                return (horariosDisponiveisDoDia agenda info (read mes) (read dia))
                                else
                                    trataOpcao3 agenda info

trataOpcao4 :: [(Int,[(Int,[(Int,Int)])])] -> (Bool, [(Int,Int)]) -> IO [(Int,[(Int,[(Int,Int)])])]
trataOpcao4 agenda info = do
                            putStr "mes:"
                            mes <- getLine
                            putStr "dia:" 
                            dia <- getLine
                            putStr "hora:"
                            hora <- getLine
                            putStr "duracao:"
                            duracao <- getLine
                            if not ((mes == "") || (dia == "") || (hora == "") || (duracao == "")) then
                                return (insert agenda info (read mes) (read dia) (read hora) (read duracao))
                                else
                                    trataOpcao4 agenda info

trataOpcao5 :: [(Int,[(Int,[(Int,Int)])])] -> (Bool, [(Int,Int)]) -> IO [(Int,[(Int,[(Int,Int)])])]
trataOpcao5 agenda info = do
                            putStr "mes:"
                            mes <- getLine
                            putStr "dia:" 
                            dia <- getLine
                            putStr "duracao:"
                            duracao <- getLine
                            if not ((mes == "") || (dia == "") || (duracao == "")) then
                                return (insereBreve agenda info (read mes) (read dia) (read duracao))
                                else
                                    trataOpcao5 agenda info

trataOpcao6 :: [(Int,[(Int,[(Int,Int)])])] -> (Bool, [(Int,Int)]) -> IO [(Int,[(Int,[(Int,Int)])])]
trataOpcao6 agenda info = do
                            putStr "mes:"
                            mes <- getLine
                            putStr "duracao:"
                            duracao <- getLine
                            if not ((mes == "") || (duracao == "")) then
                                return (insereMenor agenda info (read mes) (read duracao))
                                else
                                    trataOpcao6 agenda info
                            
trataOpcao7 :: [(Int,[(Int,[(Int,Int)])])] -> (Bool, [(Int,Int)]) -> IO [(Int,[(Int,[(Int,Int)])])]
trataOpcao7 agenda info = do
                            putStr "mes:"
                            mes <- getLine
                            putStr "duracao:"
                            duracao <- getLine
                            if not ((mes == "") || (duracao == "")) then
                                return (insereMaior agenda info (read mes) (read duracao))
                                else
                                    trataOpcao7 agenda info

trataOpcao8 :: [(Int,[(Int,[(Int,Int)])])] -> (Bool, [(Int,Int)]) -> IO [(Int,[(Int,[(Int,Int)])])]
trataOpcao8 agenda info = do
                            putStr "mes:"
                            mes <- getLine
                            putStr "dia:" 
                            dia <- getLine
                            putStr "hora:"
                            hora <- getLine
                            if not ((mes == "") || (dia == "") || (hora == "")) then
                                return (cancel agenda (read mes) (read dia) (read hora))
                                else
                                    trataOpcao8 agenda info

trataOpcao9 :: [(Int,[(Int,[(Int,Int)])])] -> (Bool, [(Int,Int)]) -> IO [(Int,[(Int,[(Int,Int)])])]
trataOpcao9 agenda info = do
                            putStr "mes Antigo:"
                            ames <- getLine
                            putStr "dia Antigo:" 
                            adia <- getLine
                            putStr "hora Antigo:"
                            ahora <- getLine
                            putStr "mes:"
                            mes <- getLine
                            putStr "dia:" 
                            dia <- getLine
                            putStr "hora:"
                            hora <- getLine
                            putStr "duracao:"
                            duracao <- getLine
                            if not ((ames == "") || (adia == "") || (ahora == "") || (mes == "") || (dia == "") || (hora == "") || (duracao == "")) then
                                return (reagendar agenda info (read ames) (read adia) (read ahora) (read mes) (read dia) (read hora) (read duracao))
                                else
                                    trataOpcao9 agenda info

boolToString :: Bool -> [Char]
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
                                                                        do
                                                                            gravaAgenda agenda
                                                                            return agenda
                                                                        else
                                                                            return agenda
    
loop :: [(Int,[(Int,[(Int,Int)])])] -> (Bool, [(Int,Int)]) -> IO ()
loop agenda infos = do
                    putStrLn (show agenda)
                    imprimeOpcoes
                    op <- getLine
                    if op == "0" then
                        return ()
                        else
                            do                 
                                agenda <- trataOpcao agenda infos op
                                loop agenda infos

main :: IO ()
main = do
        agenda <- lerAgenda
        infos <- lerInfo
        loop agenda infos
        