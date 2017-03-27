{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}


module Main where

import qualified System.Environment as SE
import qualified Data.Maybe as DM 
import Text.ParseCSV as TP
import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import qualified Data.Either as DE

appName :: String
appName  = "DEFTOIDC"

appVers :: String
appVers = "0.1.0"

appDate :: String
appDate = "[Compiled " ++ __DATE__ ++ " " ++ __TIME__ ++ "]"

appDesc :: String
appDesc  = "NC MX-5 Definition to IDC Conversion Utility"

appCopy :: String
appCopy  = "Copyright (C) 2017 by Sean Person"

appUsage :: String
appUsage = "Usage: deftoidc <filepath>"


data Values = Values
    { offset      :: String
    , category    :: String
    , description :: String
    } deriving (Show)


data Def = Def
    { calId  :: String
    , header :: [String]
    , values :: [Values]
    } deriving (Show)


replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c


stripChars :: String -> String -> String
stripChars [] _ = ""
stripChars (a:[]) b = (replace a '_' b)
stripChars (a:as) b = stripChars as (replace a '_' b)


parseValues :: [DT.Text] -> Values
parseValues s = Values 
        { offset      = col0
        , category    = col2
        , description = col3
        } where
            cols = map DT.unpack s
            col0 = cols !! 0
            col2 = cols !! 2
            col3 = stripChars " -|><()/," $ cols !! 3


rowsToMaybes :: Values -> Maybe Values
rowsToMaybes a
    | offset a == "" = Nothing
    | otherwise = Just a


toDef :: [[DT.Text]] -> Def
toDef rows = Def 
    { calId     = DT.unpack . head $ rows !! 0
    , header    = map DT.unpack $ rows !! 1
    , values    = DM.catMaybes . map rowsToMaybes . map parseValues $ drop 2 rows
    }


makeName :: Values -> String
makeName a = "MakeNameEx(0x" 
                ++ (offset a) 
                ++ "," 
                ++ "\"" 
                ++ (description a)
                ++ "_"
                ++ (offset a) 
                ++ "\",SN_CHECK);\n"

wrapIdc :: String -> String
wrapIdc a = "// defToIdc\n#include <idc.idc>\nstatic defToIdc()\n{\n" ++ a ++ "\n}"


main :: IO()
main = do 

    putStrLn $ appName ++ " " ++ appVers ++ " " ++ appDate ++ "\n" ++ appDesc ++ "\n" ++ appCopy ++ "\n"
    
    args <- SE.getArgs
    
    if (length args /= 1)
        then do
            putStrLn appUsage
        else do
            -- csvText <- DTIO.readFile $ "/Users/SP/Downloads/LFLPEB_Base_Def_17-0324 - Sheet1.csv"
            csvText <- DTIO.readFile $ head args  
            DE.either (\err -> putStrLn err) 
                      (\csv -> do 
                                let def = toDef csv
                                let outputFilename = (calId def) ++ ".idc"
                                writeFile (outputFilename) $ wrapIdc . concatMap makeName $ values def
                                putStrLn $ "Output: " ++ outputFilename
                      )
                      $ parseCSV csvText
