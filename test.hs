import System.IO
import Data.Char

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