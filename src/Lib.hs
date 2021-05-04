{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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

data Ergebnis = Ergebnis {
  einfacheErgebnis :: EinfacheErgebnis,
  komplexeErgebnis :: KomplexeErgebnis
} 

newtype EinfacheErgebnis = EinfacheErgebnis {
  wurzelWert' :: Int
} deriving (Eq, Show)

data KomplexeErgebnis = KomplexeErgebnis {
  radikand :: Int,
  multiplicator :: Int
} deriving (Eq, Show)

data StandardWerte = StandardWerte {
  wurzelWerte :: [Int],
  multiplikators :: [Int]
} deriving (Eq, Show)

data Root = Root {
  wurzelWert :: Int, -- wird nicht verwendet
  radicand :: Int
} deriving (Eq, Show)

-- Hauptfunktion welche die Schnittstelle difeniert
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
-- 16 + 9 = [4,9,16,25]
-- 25 + 11 = [4,9,16,25,36]
-- 36 + 13 = [4,9,16,25,36,49]

--Sonderfall
-- radicand 2 = sqrt(2)

berechneStandardWerte :: Int -> StandardWerte
berechneStandardWerte radikand = 
  StandardWerte [2 .. radikand] (berechneWurzelWerte ungeradeZahlen)
  where 
    ungeradeZahlen :: [Int]
    ungeradeZahlen = filter odd [1 .. radikand]
    berechneWurzelWerte :: [Int] -> [Int]
    berechneWurzelWerte [x] = []
    berechneWurzelWerte (x:y:xs)
      | summe <= radikand = summe : berechneWurzelWerte xs
      | otherwise         = []
      where 
        summe = x + y
    

calcStandartRoots :: Int  -> [Int]
calcStandartRoots radicand = calc listOfOddNumbers
    where listOfOddNumbers :: [Int] -- berchne Ungerade Zahlen
          listOfOddNumbers = filter odd [1 .. radicand]
          calc :: [Int] -> [Int] -- addire recursive erste 2 Elemente und füge Sie zu der neue Liste
          calc [x] = []
          calc (x:y:xs)
            | summe <= radicand = summe : calc (summe : xs)
            | otherwise         = []
            where summe = x + y

appendResultOnStandartRoots :: [Int] -> [Int] -> [Root]
appendResultOnStandartRoots [] _ = []
appendResultOnStandartRoots _ [] = []
appendResultOnStandartRoots (x:xs) (y:ys) = Root x y : appendResultOnStandartRoots xs ys

simpleSerchInStandartRoots :: Int -> [Root] -> [Root]
simpleSerchInStandartRoots radicand_ [] = []
simpleSerchInStandartRoots radicand_ xs = filter (\root -> radicand_ == radicand root) xs

-- quotRem -> (It returns a tuple: (result of integer division, reminder) )
-- versuche den wurzel restlos mit jedem standart Wurzel zu teilen

complexSerchOfExactResult :: Int -> [Root] -> [Root]
complexSerchOfExactResult radicand_ [] = [Root 0 radicand_]
complexSerchOfExactResult radicand_ (x:xs)
                            | snd resFromQout == 0 = [x, uncurry Root resFromQout]
                            | snd resFromQout > 0  = complexSerchOfExactResult radicand_ xs
                            | otherwise            = [x]
                            where resFromQout = quotRem radicand_ (radicand x)