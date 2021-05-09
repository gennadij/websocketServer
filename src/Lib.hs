module Lib
(
  berechneWurzel,
  Ergebnis ( .. )
) where

{------------------------------------------------------------------------------

Exacte Berechnung der Wurzel
Die Berechnung beruht auf das Prenzip der Summe ungeraden Reihen.

Beispiel sqrt(50)
1 + 3 = [4]
4 + 5 = [4,9]
9 + 7 = [4,9,16]
16 + 9 = [4,9,16,25]
25 + 11 = [4,9,16,25,36]
36 + 13 = [4,9,16,25,36,49]

1. Berschne ungerade Reihe bis 50
2. Berechne in der ungerade Reihe die summe der ersten zwei Elemente und hänge
   diese an der neue Reihe (siehe oben). 
3. Parallel zu der o.g. Reihe wird zu jedem Wert einen Wert aus der einfache 
   Reihe angehaengt z.B. [(2,4),(3,9),(4,16)]. In weiteren Beschreibung wird 
   diese Reihe als Wurzelwert_Radikand genannt.
   Funktion : berechneStandardwerte Int -> StandardWerte
4. Berechne einfache Wurzelwert z.B. sqrt(25) = 5
   Dazu wird einfach in der Reihe Wurzelwert_Radikand der zweite Wert 
   (Radikand) verglichen und der erste Wert (Wurzelwert) als Ergebnis
   ausgegeben
   Funktion : 
5. Berechnung des komplexen Wurzelwertes z.B. sqrt(50) = 5 * sqrt(2)

------------------------------------------------------------------------------}
data Ergebnis = Ergebnis {
  wurzelWert :: Int,
  multiplikator :: Int,
  berechnebar :: Bool 
 } deriving (Eq, Show)

newtype EinfacheErgebnis = EinfacheErgebnis {
  wurzelWert_ :: Int
} deriving (Eq, Show)

data KomplexeErgebnis = KomplexeErgebnis {
  radikand_ :: Int,
  multiplikator_ :: Int
} deriving (Eq, Show)

type Wurzelwert = Int
type Radikand   = Int

data StandardWerte = StandardWerte {
  radikands :: [Int],
  wurzelwerte_radikands :: [(Wurzelwert, Radikand)]
} deriving (Eq, Show)

-- Hauptfunktion welche die Schnittstelle difeniert
berechneWurzel :: Int -> Ergebnis
berechneWurzel radikand__
  | multiplikator_ komplexeWurzelwert == 0 && radikand_ komplexeWurzelwert == 0 = 
    Ergebnis 0 0 False 
  | einfacheWurzelwert == 0 = 
    Ergebnis (radikand_ komplexeWurzelwert) (multiplikator_ komplexeWurzelwert) True
  | otherwise = Ergebnis einfacheWurzelwert 0 True
    where standardWerte = berechneStandardwerte radikand__
          einfacheErgebnis = berechneEinfacheWurzelwert radikand__ standardWerte
          komplexeWurzelwert = berechneKomplexeWurzelwert radikand__ standardWerte
          einfacheWurzelwert = wurzelWert_ einfacheErgebnis

berechneStandardwerte :: Int -> StandardWerte
berechneStandardwerte radikand =
  StandardWerte 
    (berechneWurzelWerte ungeradeZahlen) 
    (zip [2 .. radikand `quot` 2] (berechneWurzelWerte ungeradeZahlen)) 
  where
    ungeradeZahlen :: [Int]
    ungeradeZahlen = filter odd [1 .. radikand]
    berechneWurzelWerte :: [Int] -> [Int]
    berechneWurzelWerte [x] = []
    berechneWurzelWerte (x:y:xs)
      | summe <= radikand = summe : berechneWurzelWerte (summe : xs)
      | otherwise         = []
      where
        summe = x + y


berechneEinfacheWurzelwert :: Int -> StandardWerte -> EinfacheErgebnis
berechneEinfacheWurzelwert radikand_ standardWerte
  | null (radikands standardWerte) = EinfacheErgebnis 0
  | null einfacheWurzelWert          = EinfacheErgebnis 0
  | otherwise                        = EinfacheErgebnis (fst (head einfacheWurzelWert))
                                      where 
                                        einfacheWurzelWert = 
                                          filter 
                                            (\radikand -> radikand_ == snd radikand) 
                                            (wurzelwerte_radikands standardWerte)

-- quotRem -> (It returns a tuple: (result of integer division, reminder) )
berechneKomplexeWurzelwert :: Int -> StandardWerte -> KomplexeErgebnis
berechneKomplexeWurzelwert radikand_ standardWerte
  | null wW    = KomplexeErgebnis 0 0
  | otherwise  = KomplexeErgebnis  (snd $ radicand wW) (fst $ radicand wW)
                    where
                      wW :: [(Int, Int)]
                      wW = wurzelwerte_radikands standardWerte
                      radicand :: [(Wurzelwert, Radikand)] -> (Int, Int)
                      radicand [] = (0, 0)
                      radicand (x:xs)
                        | snd ergebnisVonQuotRem == 0 = (fst x, fst ergebnisVonQuotRem)
                        | snd ergebnisVonQuotRem > 0  = radicand xs
                        | otherwise                   = (fst x, 0)
                        where ergebnisVonQuotRem = quotRem radikand_ (snd x)

