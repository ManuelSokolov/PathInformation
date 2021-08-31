------------------------------------------------------------------------------------------------------------------------
-- Trabalho realizado por: Joao Lopes numero: 51053 ; Manuel Ravasqueira numero: 51963; Guilherme Soares numero: 51798
-- Principios Progamacao     2019/2020     Projeto
------------------------------------------------------------------------------------------------------------------------
--module Ficheiros (funcoes para tratamento dos dados do ficheiro csv)
module Ficheiros
    (retiraColunas
,    retiraVirgulas
,    listaLatLong
,    listaLatLongName
    ) where

-- Retira a coluna x da tabela
retiraColunas :: [[Char]] -> Int -> [String]
retiraColunas [] _ = []
retiraColunas (x:xs) col = [(lines $ retiraVirgulas x 0)!!col] ++ retiraColunas xs col

-- Retira as virgulas de um [CHAR] convertendo as em \n para aplicar o lines
retiraVirgulas :: Num a => [Char] -> a -> [Char]
retiraVirgulas [] _ = []
retiraVirgulas (x:xs) i
    | x /= ',' = [x]++retiraVirgulas xs (i+1)
    | otherwise    =['\n']++retiraVirgulas xs (i+1)

-- Cria uma lista de tuplos de 2 com a latitude e a longitude de cada linha num Ponto do Percurso
listaLatLong :: [a] -> [b]  -> [(a, b)]
listaLatLong [] [] = []
listaLatLong (x:lat) (y:long) = [(x,y)] ++ listaLatLong lat long


-- Cria uma lista de tuplos de 3 com a latitude, a longitude e o nome de cada linha num Ponto de Interesse
listaLatLongName :: [a] -> [b] -> [c] -> [(a, b, c)]
listaLatLongName [] [] [] = []
listaLatLongName (x:lat) (y:long) (z:nomes) = [(x,y,z)] ++ listaLatLongName lat long nomes
