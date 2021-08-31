------------------------------------------------------------------------------------------------------------------------
-- Trabalho realizado por: Joao Lopes numero: 51053 ; Manuel Ravasqueira numero: 51963; Guilherme Soares numero: 51798
-- Principios Progamacao     2019/2020     Projeto
------------------------------------------------------------------------------------------------------------------------

-- modulo Funcoes (funcoes que sao aplicadas no ficheiro Main com os dados do csvs)
module Funcoes
    (ganhoAcumuladoTotal
,    ganhoAcumuladoTotalPMetro
,    categoria
,    pontosInteresse
,    roundPara3Casas
    ) where


-- Devolve uma lista com os nomes dos Pontos de Interesse pelos quais passa pelo menos um ponto da lista Pontos do Percurso
pontosInteresse :: [(String, String)] -> [(String, String, String)] -> [String]
pontosInteresse _ [] = []
pontosInteresse percurso (a:listaPI)
    | [checkPonto a percurso] == [] = pontosInteresse percurso listaPI
    | otherwise = checkPonto a percurso ++ pontosInteresse percurso listaPI


-- Verifica se um ponto da lista dos Pontos de Percurso esta dentro do raio de vizinhanca de um ponto da lista Ponto de Interesse e devolve o nome do Ponto de Interesse
checkPonto :: (String, String, String) -> [(String, String)] -> [String]
checkPonto b [] = []
checkPonto b (x:lista)
        | (dist (sToFloat (fst x) , sToFloat (snd x)) (sToFloat(getElemento b 0),sToFloat(getElemento b 1)))  <=  resolucao(getElemento b 0) = [getElemento b 2]
        |  otherwise = checkPonto b lista


-- Transforma uma String num Float
sToFloat :: String -> Float
sToFloat a = read a :: Float

-- Retira o elemento de um tuplo de 3 consoante a posicao 0,1 ou 2 escolhida
getElemento :: (Eq a, Num a) => (p, p, p) -> a -> p
getElemento (a,b,c) pos
    | pos == 0 =  a
    | pos == 1 =  b
    | pos == 2 =  c

-- callcula a resolucao que se deve considerar para saber qual  raio da vizinhanca
resolucao :: Floating a => [Char] -> a
resolucao lat = 10**(-numeroCasasDecimais(lat))

-- Calcula o numero de casas decimais de umc certo numero -- o contador tem de comecar a 0
numeroCasasDecimais :: Num a => [Char] -> a
numeroCasasDecimais xs = numeroCasasDecimaisAux xs 0 0

-- Funcao auxiliar para calcular o numero de casas decimais
numeroCasasDecimaisAux :: (Eq t, Num t, Num a) => [Char] -> t -> a -> a
numeroCasasDecimaisAux [] antesDoZero contador = contador
numeroCasasDecimaisAux (x:xs) antesDoZero contador
    | x == '.' = numeroCasasDecimaisAux xs 1 contador
    | antesDoZero == 0 = numeroCasasDecimaisAux xs antesDoZero contador
    | otherwise        = numeroCasasDecimaisAux xs 1 (contador+1)


-- Arredonda para 3 casas decimais um certo numero
roundPara3Casas :: (RealFrac a1, Fractional a2) => a1 -> a2
roundPara3Casas x = fromIntegral(round(x*1000))/1000


-- Calcula o ganho acumulado total
ganhoAcumuladoTotal :: [Float] -> Float
ganhoAcumuladoTotal xs = ( sum (filter (> 0) (diferenca_Altitude xs)))


-- Calcula o ganho acmulado total por metro
ganhoAcumuladoTotalPMetro :: [Float] -> [Float] -> [Float] -> Float
ganhoAcumuladoTotalPMetro xs lat long =  ((ganhoAcumuladoTotal xs) / distancia lat long)


-- Atribui uma categoria (letra A, B, C, D) a um percurso consoante o ganho acumulado e a distancia realizada
categoria :: (Ord a, Floating a) => [Float] -> [a] -> [a] -> String
categoria g lat long
    |ganhoAcumuladoTotal g < 500 && distancia lat long < 10000 =  "A"
    |(ganhoAcumuladoTotal g >= 500 && ganhoAcumuladoTotal g < 800) && distancia lat long < 12000 =  "B"
    |(ganhoAcumuladoTotal g >= 800 && ganhoAcumuladoTotal g < 1500) && distancia lat long <= 15000 =  "C"
    |ganhoAcumuladoTotal g >= 1500 && distancia lat long > 15000 =  "D"


-- Calcula a diferenca de altitude num percurso
diferenca_Altitude :: Num a => [a] -> [a]
diferenca_Altitude [] = []
diferenca_Altitude [x] = []
diferenca_Altitude (x:y:xs) = [y-x] ++ diferenca_Altitude (y:xs)


--Calcula a distancia de um percurso
-- Eh necessario multiplicar po 85000 (metros) pq o resultado da em graus
--e cada graus corresponde a 85km (85000 metros - usamos sempre os metros nas contas)
distancia :: Floating a => [a] -> [a] -> a
distancia xs ys =  (foldl(\acc x -> acc + x) 0 (lista xs ys)) * 85000


-- Calcula a distancia entre 2 pontos
dist :: Floating a => (a, a) -> (a, a) -> a
dist x y = sqrt((fst x - fst y)^2 + (snd x - snd y)^2)


---Cria uma lista com as distancias das coordenadass da latitude e longitude
lista :: Floating a => [a] -> [a] -> [a]
lista (x:[]) (a:[])= [sqrt (x^2 + a^2)]
lista (x:y:[]) (a:b:[])= [dist (x,a) (y,b)]
lista (x:y:xs) (a:b:ys) = [dist (x,a) (y,b)] ++ lista (y:xs) (b:ys)
