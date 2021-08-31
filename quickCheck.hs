{-
Suporte às notas "Teste de funções com QuickCheck"

Vasco Thudichum Vasconcelos
Princípios de Programação 

Universidade de Lisboa
Faculdade de Ciências
Departamento de Informática
Licenciatura em Engenharia Informática

Dezembro 2015
-}

import Test.QuickCheck
import qualified Data.List as List
import Control.Monad

reverse' :: [a] -> [a]
reverse' xs = rev xs []
  where rev (x:xs) acc = rev xs (x:acc)
        rev [] acc = acc

prop_reverse_length :: [Int] -> Bool
prop_reverse_length xs = length (reverse'' xs) == length xs

prop_reverse_last :: [Int] -> Bool
prop_reverse_last xs = head xs == last (reverse' xs)

prop_reverse_last' :: [Int] -> Property
prop_reverse_last' xs = not (null xs) ==> head xs == last (reverse' xs)

prop_reverve_singleton :: Int -> Bool
prop_reverve_singleton x = [x] == [x]

prop_reverse_concat :: [Int] -> [Int] -> Bool
prop_reverse_concat xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_reverse_reverse :: [Int] -> Bool
prop_reverse_reverse xs = xs == reverse (reverse xs)

reverse'' :: [a] -> [a]
reverse'' xs = rev xs []
  where rev (x:xs) acc = rev xs (x:acc)
        rev [] acc = []

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x <= y then x : y : ys else y : insert x ys

prop_sort_is_sort :: [Int] -> Bool
prop_sort_is_sort xs = sort xs == List.sort xs

ordered :: Ord a => [a] -> Bool
ordered []       = True
ordered [x]      = True
ordered (x:y:xs) = (x <= y) && ordered (y:xs)

prop_sort_ordered :: [Int] -> Bool
prop_sort_ordered xs = ordered (sort xs)

sameElems :: Eq a => [a] -> [a] -> Bool
xs `sameElems` ys = xs List.\\ ys == ys List.\\ xs

prop_sort_same_elems :: [Int] -> Bool
prop_sort_same_elems xs = sort xs `sameElems` xs

prop_insert_same_elems :: Int -> [Int] -> Bool
prop_insert_same_elems x xs = insert x xs `sameElems` (x:xs)

prop_insert_ordered :: Int -> [Int] -> Property
prop_insert_ordered x xs = ordered xs ==> ordered (insert x xs)

prop_insert_ordered' :: Int -> [Int] -> Property
prop_insert_ordered' x xs = ordered xs ==>
                            classify (null xs) "trivial" $
                              ordered (insert x xs)

prop_insert_ordered'' :: Int -> [Int] -> Property
prop_insert_ordered'' x xs = ordered xs ==>
                            collect (length xs) $
                              ordered (insert x xs)

args = Args {
     replay = Nothing,
     maxSuccess = 10,
     maxDiscardRatio = 100,
     maxSize = 200,
     chatty = True,
     maxShrinks = 100}

test = (quickCheck . verbose) prop_sort_same_elems

-- Semaforos

data Semaforo = Verde | Amarelo | Encarnado deriving (Eq, Show)

instance Arbitrary Semaforo where
  arbitrary = elements [Verde, Amarelo, Encarnado]

{-
instance Arbitrary Semaforo where
  arbitrary = oneof [return Verde, return Amarelo, return Encarnado]

instance Arbitrary Semaforo where
  arbitrary = do
    n <- choose (1, 3) :: Gen Int
    return $ case n of
      1 -> Verde
      2 -> Amarelo
      3 -> Encarnado
-}

seguinte :: Semaforo -> Semaforo
seguinte Verde = Amarelo
seguinte Amarelo = Encarnado
seguinte Encarnado = Verde

anterior :: Semaforo -> Semaforo
anterior Amarelo = Verde
anterior Encarnado = Amarelo
anterior Verde = Encarnado

prop_semaforo_circular s = (seguinte . seguinte . seguinte) s == s

-- Pares

data Par a b = Par a b deriving (Eq, Show)

{-
instance (Arbitrary a, Arbitrary b) => Arbitrary (Par a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Par x y
-}

instance (Arbitrary a, Arbitrary b) => Arbitrary (Par a b) where
  arbitrary = liftM2 Par arbitrary arbitrary

-- Documentos

data Doc = Vazio | Frase String | Concat Doc Doc deriving (Eq, Show)

{-
instance Arbitrary Doc where
  arbitrary = do
    n <- choose (1, 3) :: Gen Int
    case n of
      1 -> return Vazio
      2 -> do s <- arbitrary
              return $ Frase s
      3 -> do d1 <- arbitrary
              d2 <- arbitrary
              return $ Concat d1 d1
-}

instance Arbitrary Doc where
  arbitrary = oneof [
    return Vazio,
    liftM Frase arbitrary,
    liftM2 Concat arbitrary arbitrary]

dimensao :: Doc -> Int
dimensao Vazio = 0
dimensao (Frase "") = 0
dimensao (Frase _) = 1
dimensao (Concat d1 d2) = (dimensao d1) + (dimensao d2)

espalmar :: Doc -> [String]
espalmar d = filter (not . null) (paraLista d) where
  paraLista Vazio = []
  paraLista (Frase s) = [s]
  paraLista (Concat d1 d2) = (paraLista d1) ++ (paraLista d2)

prop_dimensao_espalmar d = length (espalmar d) == dimensao d

nos :: Doc -> Int
nos Vazio = 1
nos (Frase _) = 1
nos (Concat d1 d2) = 1 + (nos d1) + (nos d2)

prop_distribuicao d = collect (nos d) True
