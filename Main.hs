------------------------------------------------------------------------------------------------------------------------
-- Trabalho realizado por: Joao Lopes numero: 51053 ; Manuel Ravasqueira numero: 51963; Guilherme Soares numero: 51798
-- Principios Progamacao     2019/2020     Projeto
------------------------------------------------------------------------------------------------------------------------
-- imports

import Funcoes
import Tempo
import Ficheiros
import Test.QuickCheck
import System.Random
import Control.Monad
import System.Environment

--ler argumentos da consola
main = do
      args <- getArgs
      if (args !! 0 == "-t") then testes else
        do
        bairrosAntigos <- readFile (args!!0)                    --"BairrosAntigosLisboa.csv"   -- ficheiro de leitura
        pontosDeInteresse <- readFile (args!!1)                 --"LisboaPOI.csv"           -- ficheiro de leitura
        let colunasB = retiraColunas$lines bairrosAntigos       --o lines separa cada linha por [linha1,linha2,....]     -- extrai colunas do primeiro ficheiro
        let colunaPI = retiraColunas$lines pontosDeInteresse    --o lines separa cada linha por [linha1,linha2,....]    -- extrai colunas do segundo ficheiro
        let sTf=map(read::String->Float)                        -- transforma uma strting num float
        let categoria1 = categoria(sTf(colunasB 2)) (sTf(colunasB 0)) (sTf(colunasB 1))       -- determina a categoria
        let tempoTotal= calculoDeHoras$colunasB 3               -- calcula o tempo total
        let ganhoAckTot = round$ganhoAcumuladoTotal$sTf(colunasB 2)        -- calcula o ganho acumulado total
        let ganhoAckTotPMetro = roundPara3Casas$ganhoAcumuladoTotalPMetro (sTf(colunasB 2)) (sTf(colunasB 0)) (sTf(colunasB 1))     -- calcula o ganho acumulado total por metro
        let percurso = listaLatLong ((colunasB 0)) ((colunasB 1))                  -- devolve uma lista formado por tuplo de 2 com a latitude e longitude
        let listaPI = listaLatLongName (colunaPI 0) (colunaPI 1) (colunaPI 2)      -- devolve uma lista formado por tuplo de 3 com a latitude, longitude e nome do Ponto
        let listaDePontos = pontosInteresse percurso listaPI                       -- devolve uma lista com os nomes dos Pontos de Interesse em que existe pelo menos um ponto da lista de percurso no raio de vizinhanca de um certo ponto de interesse
        let json = "{" ++ "\n" ++ "\"Categoria\": " ++ show categoria1 ++ ",\n" ++ "\"Tempo total (m)\": " ++ show tempoTotal ++ ",\n" ++ "\"Ganho acumulado\": " ++ show ganhoAckTot ++ ",\n" ++ "\"Ganho acumulado por m\": " ++ show ganhoAckTotPMetro ++ ",\n" ++ "\"Pontos de Interesse\": " ++ show listaDePontos ++ "\n"++ "}"
        writeFile "Lisboa.json" json   -- ficheiro de escrita


data Rota = Rota {listaPontos :: [(Float, Float)]} deriving(Show)


instance Arbitrary Rota where
  arbitrary = do
  xs <- arbitrary::Gen[(Float, Float)]
  return $ criaRota xs


-- Funcao que cria uma Rota
criaRota :: [(Float, Float)] -> Rota
criaRota percurso = Rota {listaPontos = percurso}


-- Adiciona um ponto da lista Pontos de Percurso a lista Pontos Interesse
adicionaPercurso :: (Float, Float) -> Rota -> Rota
adicionaPercurso x ys = criaRota((getLista ys)++[x])


getLista :: Rota -> [(Float, Float)]
getLista (Rota listaPontos) = listaPontos

-- Teste numero 1 - O ganho acumulado é sempre positivo
prop_ganho_ac :: [Float] -> Bool
prop_ganho_ac xs =ganhoAcumuladoTotal xs >= 0


-- Teste numero 2 - Se juntarmos um ponto de um percurso à lista dos pontos de interesse a considerar,
-- aumentamos em uma unidade o comprimento da lista de pontos de interesse nesse percurso
prop_adiciona_percurso :: (Float, Float) -> Rota -> Bool
prop_adiciona_percurso x ys = (length (getLista ys))+1 == length (getLista(adicionaPercurso x ys))


-- Teste numero 3 - O tempo total é sempre positivo
prop_tempo_total :: [[Char]] -> Bool
prop_tempo_total xs =  calculoDeHoras xs >= 0


-- Testa os testes automaticamente
testes :: IO ()
testes = do
    quickCheck prop_ganho_ac
    quickCheck prop_adiciona_percurso
    quickCheck prop_tempo_total
