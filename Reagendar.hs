module Reagendar(
    reagendar
 ) where
import Disponivel
import Cancel
import Insert

reagendar :: [(Int, [(Int, [(Int, Int)])])] -> (Bool, [(Int, Int)]) -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [(Int, [(Int,[(Int,Int)])])]
reagendar [] infoCal mes dia hora nMes nDia nHora nDuracao = []
reagendar agenda infoCal mes dia hora nMes nDia nHora nDuracao = if disponivel agenda infoCal nMes nDia nHora nDuracao then
                                                                    insert (cancel agenda mes dia hora) infoCal nMes nDia nHora nDuracao
                                                                else
                                                                    agenda