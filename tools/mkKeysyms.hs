{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.List
import Data.Char
import Text.Nowdoc

dstFile :: FilePath
dstFile = "./src/Data/KeySym.hsc"

header1, header2 :: String
header1 = [nowdoc|
{-

File auto-generated from script tools/mkKeySyms.hs using the input file
/usr/include/X11/keysymdef.h and /usr/include/X11/XF86keysym.h

-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.KeySym where

import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Enum
|]

header2 = [nowdoc|
#include <keysymdef.h>
#include <XF86keysym.h>

enum "KeySym" ''CUInt [''Show, ''Eq, ''Storable] [
|]

main :: IO ()
main = do
	let	fp1 = "include/keysymdef.h"
		fp2 = "include/XF86keysym.h"
	dfs <- unlines . map (("#define " ++) . (!! 1) . words) . filter ("#ifdef" `isPrefixOf`) . lines <$> readFile fp1
	body1 <- (header1 ++) . (dfs ++) . (header2 ++) . unlines . map (mkLine . (!! 1) . words)
		. filter ("#define XK_" `isPrefixOf`) . lines <$> readFile fp1
	body2 <- unlines . appLast ((++ " ]") . init) . map (mkLine . (!! 1) . words)
		. filter (not . ("_EVDEVK" `isInfixOf`)) . filter ("#define XF86XK_" `isPrefixOf`) . lines <$> readFile fp2
	putStr body1
	putStr body2
	writeFile dstFile $ body1 ++ body2

readFiles :: [FilePath] -> IO String
readFiles fs = concat <$> readFile `mapM` fs

mkLine :: String -> String
mkLine c =
	"\t(\"" ++ appHead toUpper (camelize c) ++ "\", " ++
	"#{const " ++ c ++ "}),"

appHead :: (a -> a) -> [a] -> [a]
appHead _ [] = []
appHead f (x : xs) = f x : xs

appTail :: (a -> a) -> [a] -> [a]
appTail _ [] = []
appTail f (x : xs) = x : map f xs

appLast :: (a -> a) -> [a] -> [a]
appLast _ [] = []
appLast f [x] = [f x]
appLast f (x : xs) = x : appLast f xs

camelize :: String -> String
camelize = camelize' . separateUnderscore

camelize' :: [String] -> String
camelize' ss = concatMap (appHead toUpper . appTail toLower) ss' ++ concatMap ("_" ++) s
	where ss' = take 1 ss; s = drop 1 ss

separateUnderscore :: String -> [String]
separateUnderscore "" = [""]
separateUnderscore ('_' : cs) = "" : separateUnderscore cs
separateUnderscore (c : cs) = appHead (c :) $ separateUnderscore cs
