module Agenda.Tipos where

type Agenda = [(Mes,AgendaDias)]
type AgendaDias = [(Dia, AgendaHoras)]
type AgendaHoras = [(Hora,Duracao)]
type Mes = Int
type Dia = Int
type Hora = Int
type Duracao = Int
type NumeroDias = Int
type IsHoraAjustada = Bool
type IsValido = Bool
type IsDisponivel = Bool
type Tamanho = Int

type HoraEDuracaoAnterior = (Hora, Duracao)
type Horarios = [Hora]

type PontoDeInsercaoDiaHora = (Dia, Hora, Duracao)
type PontoDeInsercaoHora = (Hora, Duracao)


type IsBisexto = Bool
type Calendario = (IsBisexto, [(Mes, Dia)])
type DiasCalendario = [(Mes, Dia)]