{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Operations.UtilOperations where

import Data.Char


isANumber :: String -> Bool -> Bool
isANumber [] _ = True
isANumber (x:xs) True = isDigit x && isANumber xs True


splitItems :: [Char] -> [String] -> [String]
splitItems [] result = reverse result
splitItems names result = do
    let x = takeWhile (/= ',') names
    let y = dropWhile (/= ',') names
    let z = if null y then y else tail y
    splitItems z (x:result)


capitalize :: String -> String
capitalize name = toUpper (head name) : tail name