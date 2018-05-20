module LerAgenda(
    lerAgenda
)where
import System.IO 

lerHoras handle = do
                  horas <- hGetLine handle
                  return (read horas :: [(Int,Int)])

lerDia handle dias = do
                    final <- hIsEOF handle
                    if final then
                        return (reverse dias)
                        else
                            do
                                dia <- hGetLine handle
                                if dia == "" then
                                    do
                                        return (reverse dias)
                                    else
                                        do
                                            horas <- lerHoras  handle
                                            lerDia handle ((read dia,horas):dias)

lerMes handle meses = do
                        final <- hIsEOF handle
                        if final then
                            return (reverse meses)
                            else
                                do
                                    mes <- hGetLine handle
                                    if mes == "" then
                                        lerMes handle meses
                                        else
                                            do
                                                dias <- lerDia handle []
                                                lerMes handle ((read mes,dias):(meses))

lerAgenda :: IO [(Int, [(Int, [(Int, Int)])])]
lerAgenda = do
            handle <- openFile "agenda.txt" ReadMode
            agenda <- lerMes handle []
            return agenda