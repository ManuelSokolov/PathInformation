------------------------------------------------------------------------------------------------------------------------
-- Trabalho realizado por: Joao Lopes numero: 51053 ; Manuel Ravasqueira numero: 51963; Guilherme Soares numero: 51798
-- Principios Progamacao     2019/2020     Projeto
------------------------------------------------------------------------------------------------------------------------

-- modulo Tempo (funcoes que sao aplicadas no ficheiro Main)
module Tempo
    (calculoDeHoras
    ) where

------------------------------------------------------------------------------------------------------------------------

-- Devolve o primeiro elemento da lista
inicioTempo :: [a] -> a
inicioTempo xs = head xs


-- Devolve o ultimo elemento da lista
fimTempo :: [a] -> a
fimTempo xs = last xs


-- Calcula as horas do primeiro elemento da lista
horasI :: [[Char]] -> Double
horasI xs =  read ([(inicioTempo xs) !! 0] ++ [(inicioTempo xs) !! 1]) ::Double


-- Calcula as horas do ultimo elemento da lista
horasF :: [[Char]] -> Double
horasF xs =  read ([(fimTempo xs) !! 0] ++ [(fimTempo xs) !! 1]) ::Double


-- Calcula os minutos do primeiro elemento da lista
minutosI :: [[Char]] -> Double
minutosI xs =  read ([(inicioTempo xs) !! 3] ++ [(inicioTempo xs) !! 4] ) ::Double


-- Calcula os minutos do ultimo elemento da lista
minutosF :: [[Char]] -> Double
minutosF xs =  read ([(fimTempo xs) !! 3] ++ [(fimTempo xs) !! 4] ) ::Double


-- Calcula os segundos do primeiro elemento da lista
segundosI :: [[Char]] -> Double
segundosI xs =  read ([(inicioTempo xs) !! 6] ++ [(inicioTempo xs) !! 7] ) ::Double


-- Calcula os segundos do ultimo elemento da lista
segundosF :: [[Char]] -> Double
segundosF xs =  read ([(fimTempo xs) !! 6] ++ [(fimTempo xs) !! 7] ) ::Double


-- Calcula os minutos totais que passaram desde as 00:00:00 ate ao primeiro elemento da lista
primeiroValor :: [[Char]] -> Double
primeiroValor xs = horasI xs * 60 + minutosI xs + ((segundosI xs)/60)


-- Calcula os minutos totais que passaram desde as 00:00:00 ate ao ultimo elemento da lista
segundoValor :: [[Char]] -> Double
segundoValor xs = horasF xs * 60 + minutosF xs + ((segundosF xs)/60)


-- Calcula os minutos entre o primeiro e o ultimo elemento da lista
calculoDeHoras :: Integral b => [[Char]] -> b
calculoDeHoras [] = 0
calculoDeHoras xs = if (length xs/=0 && valoresCertos xs == True && verificaHoras xs == True) then round(abs (primeiroValor xs - segundoValor xs))
                  else 0


-- Confirma que as horas tem de ser um valor entre 0 e 23, os minutos entre 0 e 59 e os segundos entre 0 e 59, caso aconteca passar em todos os casos devolve True, caso contrario devolve False
verificaHoras :: [[Char]] -> Bool
verificaHoras xs = if ((segundosI xs <=59 && segundosI xs>= 0) && (minutosI xs <= 59 && minutosI xs>= 0) && (horasI xs >= 0 && horasI xs <= 23) && (segundosF xs <=59 && segundosF xs>= 0) && (minutosF xs <= 59 && minutosF xs>= 0) && (horasF xs >= 0 && horasF xs <= 23)) then True else False


-- Caso se verifique um destes casos devolve False caso contrario True
valoresCertos :: [[Char]] -> Bool
valoresCertos [] = True
valoresCertos (x:xs)--hora 24 e 1
  |x=="" || (length x >8) || (length x <8) || not (isDigit (x!!0))
  || not (isDigit (x!!1)) || not (isDigit (x!!3)) || not (isDigit (x!!4))
  || not (isDigit (x!!6)) || not (isDigit (x!!7))  = False
  |otherwise = valoresCertos xs

  -- Seleciona ASCII digits, i.e. @\'0\'@..@\'9\'@.
isDigit :: Char -> Bool
isDigit c =  c >= '0' && c <= '9'
------------------------------------------------------------------------------------------------------------------------
