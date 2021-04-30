module Lib
( 
  calcExactRoot_2,
  getFirst,
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
  radikand :: Int 
} deriving (Eq, Show)

getFirst :: Root -> Int
getFirst (Root a b) = a

getSecond :: Root -> Int
getSecond (Root a b) = b

calcExactRoot_2 :: Int -> [Root] 
calcExactRoot_2 radicand
  | null result = complexSerchOfExactResult_2 radicand (giveRoots_2 radicand)
  | otherwise = result  
    where result = simpleSerchInStandartRoots_2 radicand (giveRoots_2 radicand)

giveRoots_2 :: Int -> [Root]
giveRoots_2 radicand = appendResultOnStandartRoots_2 [2 .. radicand] (calcStandartRoots radicand)

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
appendResultOnStandartRoots_2 :: [Int] -> [Int] -> [Root]
appendResultOnStandartRoots_2 [] _ = []
appendResultOnStandartRoots_2 _ [] = []
appendResultOnStandartRoots_2 (x:xs) (y:ys) = Root x y : appendResultOnStandartRoots_2 xs ys

-- Root_2
simpleSerchInStandartRoots_2 :: Int -> [Root] -> [Root]
simpleSerchInStandartRoots_2 radicand [] = []
simpleSerchInStandartRoots_2 radicand xs = filter (\(Root a b) -> radicand == b) xs

-- quotRem -> (It returns a tuple: (result of integer division, reminder) )
-- versuche den wurzel restlos mit jedem standart Wurzel zu teilen

complexSerchOfExactResult_2 :: Int -> [Root] -> [Root]
complexSerchOfExactResult_2 radicand [] = [Root 0 radicand]
complexSerchOfExactResult_2 radicand (x:xs)
                            | snd resFromQout == 0 = [x, uncurry Root resFromQout]
                            | snd resFromQout > 0  = complexSerchOfExactResult_2 radicand xs
                            | otherwise              = [x]
                            where resFromQout = quotRem radicand (root x)
                                                where root :: Root -> Int
                                                      root (Root _ b) = b

showResult :: Int -> [Root] -> String
showResult radicand [x] = "Ergebnis von sqrt(" ++ show radicand ++ ") ist " ++ show (getFirst x) ++ "."
showResult radicand [x,y] = "Ergebnis von sqrt(" ++ show radicand ++ ") ist " ++ show (getFirst x) ++ "*sqrt(" ++ show (getFirst y) ++ ")."