module Lib
( 
  calcExactRoot,
  Root( .. )
) where

{-
Exacte berechnung der Wurzel
Die Berechnung beruht auf das Prenzip der Summe ungeraden Reihen.
Beispiel:
sqrt(4) = 2 -> 1 + 3 
sqrt(9) = 3 -> 1 + 3 + 5
sqrt(16) = 4 -> 1 + 2 + 3 + 4

Beispiel sqrt(50)

1. Berschne ungerade Zahlen bis 50

-}
type WurzelWert = Int
type Radikand = Int

data Root = Root {
  wurzelWert :: Int,
  radicand :: Int 
} deriving (Eq, Show)

calcExactRoot :: Int -> [Root] 
calcExactRoot radicand
  | null result = complexSerchOfExactResult radicand (giveRoots radicand)
  | otherwise = result  
    where result = simpleSerchInStandartRoots radicand (giveRoots radicand)

giveRoots :: Int -> [Root]
giveRoots radicand = appendResultOnStandartRoots [2 .. radicand] (calcStandartRoots radicand)

-- beispiel 50 

-- berechne alle moegliche Wurzeln bis zu radicand
-- radicand -> Liste ungerade Zahlen -> Liste mit Wurzeln
-- 1 + 3 = [4]
-- 4 + 5 = [4,9]
-- 9 + 7 = [4,9,16]

calcStandartRoots :: Int  -> [Int]
calcStandartRoots radicand = calc radicand listOfOddNumbers
    where listOfOddNumbers :: [Int] -- berchne Ungerade Zahlen
          listOfOddNumbers = filter odd [1 .. radicand]
          calc :: Int -> [Int] -> [Int] 
          calc radicand (x:y:xs)
            | summe <= radicand = summe : calc radicand (summe : xs)
            | otherwise         = []
            where summe = x + y

-- Root_2
appendResultOnStandartRoots :: [Int] -> [Int] -> [Root]
appendResultOnStandartRoots [] _ = []
appendResultOnStandartRoots _ [] = []
appendResultOnStandartRoots (x:xs) (y:ys) = Root x y : appendResultOnStandartRoots xs ys

-- Root_2
simpleSerchInStandartRoots :: Int -> [Root] -> [Root]
simpleSerchInStandartRoots radicand [] = []
simpleSerchInStandartRoots radicand xs = filter (\(Root a b) -> radicand == b) xs

-- quotRem -> (It returns a tuple: (result of integer division, reminder) )
-- versuche den wurzel restlos mit jedem standart Wurzel zu teilen

complexSerchOfExactResult :: Int -> [Root] -> [Root]
complexSerchOfExactResult radicand [] = [Root 0 radicand]
complexSerchOfExactResult radicand (x:xs)
                            | snd resFromQout == 0 = [x, uncurry Root resFromQout]
                            | snd resFromQout > 0  = complexSerchOfExactResult radicand xs
                            | otherwise              = [x]
                            where resFromQout = quotRem radicand (root x)
                                                where root :: Root -> Int
                                                      root (Root _ b) = b