module LerInfos(
    lerInfo
)where
import System.IO
_nextInt :: [Char] -> [Char] -> [Char]
_nextInt [] numero = numero
_nextInt (c:linha) numero = if c == ' '
                            then numero
                            else _nextInt linha (c:numero) 

_infoadd :: [(Int, Int)] -> Int -> [Char] -> [(Int, Int)]
_infoadd infos mes buff = if numChar == []
                            then if (length buff) == 0
                                then infos
                                else _infoadd infos mes (drop 1 buff)
                            else
                                _infoadd ((mes, (read numChar :: Int)):infos) mes (drop (length numChar) buff)
                            where
                                numChar = reverse (_nextInt buff [])
                
_lerInfo :: Handle -> Int -> [(Int,Int)] -> IO [(Int, Int)]
_lerInfo handle mes infos =  do
                                eof <- hIsEOF handle
                                if eof
                                then return (infos) 
                                else do 
                                        buff <- hGetLine handle
                                        _lerInfo handle (mes+1) (_infoadd infos mes buff)

lerInfo :: IO (Bool, [(Int,Int)])
lerInfo = do
                handle <- openFile "calendario.txt" ReadMode
                op <- hGetLine handle
                _infos <- _lerInfo handle 1 []
                hClose handle
                if op == "False" then
                        return (False, reverse (_infos))
                else
                        return (True, reverse (_infos))