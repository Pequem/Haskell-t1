module GravaAgenda(
    gravaAgenda
)where
import System.IO

_gravaHoras :: Handle -> [(Int,Int)] -> IO ()
_gravaHoras handle horas = do
                            hPutStrLn handle (show horas)
                            return ()

_gravaDias :: Handle -> [(Int, [(Int,Int)])] -> IO ()
_gravaDias handle [] = do return ()
_gravaDias handle ((dia,horas):dias) = do
                                        hPutStrLn handle (show dia)
                                        _gravaHoras handle horas
                                        _gravaDias handle dias
                                        return ()

_gravaMeses :: Handle -> [(Int, [(Int, [(Int, Int)])])] -> IO ()
_gravaMeses handle [] = do return ()
_gravaMeses handle ((mes,dias):agenda) = do
                                            hPutStrLn handle (show mes)
                                            _gravaDias handle dias
                                            hPutStrLn handle ""
                                            _gravaMeses handle agenda
                                            return ();

gravaAgenda :: [(Int, [(Int, [(Int, Int)])])] -> IO ()
gravaAgenda agenda = do
                        handle <- openFile "agenda.txt" WriteMode
                        _gravaMeses handle agenda
                        hFlush handle
                        hClose handle
