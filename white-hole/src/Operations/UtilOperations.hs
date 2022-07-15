{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Operations.UtilOperations where

import Data.Char
import qualified System.Process as System


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

verifyCategories :: [String] -> IO Bool
verifyCategories [] = return True
verifyCategories (x:xs) = do
    result <- verifyCategories xs
    return (isANumber x True && (read x :: Integer) `elem` [1..14] && result)


convertCategories :: [String] -> [String] -> [String]
convertCategories [] result = reverse result
convertCategories (x:xs) result = do
    let categories = ["ação","suspense","romance","comédia","terror","aventura","investigação","fantasia","documentário","drama","anime","mistério","infantil","ficção científica"]
    convertCategories xs (categories !! ((read x :: Int) - 1):result)


concatenateWithComma :: [String] -> String
concatenateWithComma [] = []
concatenateWithComma (x:xs) = " " ++ x ++ "," ++ concatenateWithComma xs


clearScreenWithConfirmation :: IO ()
clearScreenWithConfirmation = do
    putStrLn ""
    putStrLn "(Aperte Enter para continuar)"
    placebo <- getLine
    System.system "clear"
    putStrLn ""


clearScreenOnly :: IO ()
clearScreenOnly = do
    System.system "clear"
    return ()