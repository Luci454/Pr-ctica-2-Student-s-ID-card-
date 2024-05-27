--Hecho por Luciana Hoyos Pérez
import Data.Char (digitToInt)
import Data.List (delete)

-- Convierte un número en una lista de dígitos
numAListaDigitos :: Int -> [Int]
numAListaDigitos n = map digitToInt (show n)

-- Función principal que procesa el número de identificación
infoId :: Int -> String
infoId num =
    let listaDigitos = numAListaDigitos num
    in if length listaDigitos == 8
        then infoIdRec listaDigitos num
        else "Código de matrícula inválido."
  
-- Función auxiliar de infoIdRec
infoIdRec :: [Int] -> Int -> String
infoIdRec listaDigitos num = (semestre listaDigitos) ++ " " ++ (category listaDigitos) ++ " " ++ (numAdmission listaDigitos) ++ " " ++ (esParTexto num)

-- Función que determina el semestre entre 2024-2 y 2026-2
semestre :: [Int] -> String   
semestre listaDigitos =
    let usar = take 3 listaDigitos
    in case usar of
        [2, 4, 2] -> "2024-2"
        [2, 5, 1] -> "2025-1"
        [2, 5, 2] -> "2025-2"
        [2, 6, 1] -> "2026-1"
        [2, 6, 2] -> "2026-2"
        _         -> "Error"

-- Convierte una lista de dígitos en un número
listaADigito :: [Int] -> Int
listaADigito = foldl (\acc x -> acc * 10 + x) 0

-- Función category que determina la categoría del programa académico
category :: [Int] -> String
category listaDigitos = 
    let usar = take 2 (drop 3 listaDigitos)
    in nicomachus usar

-- Función nicomachus 
nicomachus :: [Int] -> String
nicomachus usar =
    let num = listaADigito usar
    in nicomachusRec 1 0 num

-- Función recursiva nicomachusRec 
nicomachusRec :: Int -> Int -> Int -> String
nicomachusRec x suma num
    | x == num = if suma > num 
                then "Administrative"
                else if suma == num 
                     then "Engineering"
                     else "Humanities"
    | otherwise = let nuevoSuma = suma + (if num `mod` x == 0 then x else 0)
                  in nicomachusRec (x + 1) nuevoSuma num

-- Función numAdmission que determina el número consecutivo de admisión al programa
numAdmission :: [Int] -> String
numAdmission listaDigitos =
    let usar = drop 5 listaDigitos
    in numAdmissionRec 0 usar

-- Función recursiva numAdmissionRec
numAdmissionRec :: Int -> [Int] -> String
numAdmissionRec x usar
    | x >= length usar = "num" ++ show (listaADigito usar)
    | usar !! x == 0   = numAdmissionRec (x+1) (delete 0 usar)
    | otherwise        = "num" ++ show (listaADigito usar)

--Función esPar booleana 
esPar :: Int -> Bool
esPar = (== 0) . (`mod` 2)

--Función esParTexto que determina si el código de indentificación es par o no 
esParTexto :: Int -> String
esParTexto num =
    if esPar num == True
        then "even"
    else
        "odd"

main :: IO ()
main = do
    numId <- readLn :: IO Int
    
    let resultado = if numId > 0
                   then infoId numId
                   else "Código de matrícula inválido."

    putStrLn resultado
    

