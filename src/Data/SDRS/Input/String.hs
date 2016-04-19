{- |
Module      :  Data.SDRS.Input.String
Copyright   :  (c) Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

String to SDRS
-}

module Data.SDRS.Input.String
(
  stringToSDRS
) where

import Data.Char (isSpace, isDigit, isAlpha)
import Data.SDRS.DataType
import Data.DRS.Input.String
import qualified Data.Map as M

stringToSDRS :: String -> SDRS
stringToSDRS s
  | felicitousBracketing s  = parseSDRS (filter (not . isSpace) s)
  | otherwise               = error "infelicitous bracketing"

parseSDRS :: String -> SDRS
parseSDRS [] = SDRS M.empty $ -1
parseSDRS s@(b:_)
  | b == '<'  = SDRS (parseMap $ takeUpToMatchingBracket Curly s') (parseLast (tail (dropUpToMatchingBracket Curly s')))
  | otherwise = error ("parseSDRS: infelicitous input string " ++ show b)
  where s' = dropOuterBrackets s


parseMap :: String -> M.Map DisVar SDRSFormula
parseMap [] = M.empty
parseMap s@(b:_)
  | b == '{'  = M.fromList $ map parseTuple (splitOn ';' (dropOuterBrackets $ takeUpToMatchingBracket Curly s)) -- nonsense to split on , here!
  | otherwise = error ("parseMap: infelicitous input string " ++ show b)
  where parseTuple :: String -> (DisVar, SDRSFormula)
        parseTuple t = (parseDV $ splitTuple !! 0, parseSDRSFormula $ splitTuple !! 1)
          where splitTuple = splitOn ':' t
        parseDV :: String -> DisVar
        parseDV [] = -1 -- FIX what should I do here
        parseDV s' 
          | all isDigit s' = read s' -- FIX error handling if it's not a number?
          | otherwise      = error ("parseDV: infelicitous input string " ++ show s' ++ "at global string " ++ s)
        parseSDRSFormula :: String -> SDRSFormula
        parseSDRSFormula [] = error "empty input string" -- FIX what to do here? throw error?
        parseSDRSFormula s'@(b':_)
          | b' == '<' = Segment $ parseDRS s' -- convert to DRS
          | b' == '(' = parseRelation (dropOuterBrackets s') -- Relation
          -- | b' == -- And / Not ...
          | otherwise = error ("parseSDRSFormula: infelicitous input string " ++ show b')
          where parseRelation :: String -> SDRSFormula
                parseRelation [] = error "parseRelation: empty input string" -- what do here?
                parseRelation s''@(b'':_)
                  | isAlpha b''  = Relation (relationFromLabel (splitRel !! 0)) (read $ splitRel !! 1) (read $ splitRel !! 2) -- FIX number errors not handled?
                  | otherwise    = error ("parseRelation: infelicitous input string " ++ show b'')
                  where splitRel = splitOn ',' s''

parseLast :: String -> DisVar
parseLast [] = -1 -- FIX
parseLast s
  | all isDigit s = read s
  | otherwise     = error "infelicitous input string"





