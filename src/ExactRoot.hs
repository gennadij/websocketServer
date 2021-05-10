module ExactRoot
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
   Funktion : berechneEinfacheWurzelwert
5. Berechnung des komplexen Wurzelwertes z.B. sqrt(50) = 5 * sqrt(2)
   Bei der komplexen Berechnung wird Iterativ versucht jeden Radikand aus der 
   Reihe Wurzelwert_Radikand mit Hilfe der quotRem funktion zu teilen.
   Wenn einen Teiler ohne Rest gefunden wird wird der Ergebnis zu dem
   unberechnetem Radikand unter Wurzel und der Wurzelwert aus der Reihe 
   Werzelwert_Radikand zu dem Multiplikator. 
   5 = Multiplikator
   sqrt(2) =  unberechneter Radikand
------------------------------------------------------------------------------}
data Ergebnis = Ergebnis {
  wurzelWert :: Int,
  multiplikator :: Int,
  berechnebar :: Bool
 } deriving (Eq, Show)

newtype EinfacheErgebnis = EE {
  ee_wurzelWert :: Int
} deriving (Eq, Show)

data KomplexeErgebnis = KE {
  ke_radikand :: Int,
  ke_multiplikator :: Int
} deriving (Eq, Show)

type Wurzelwert = Int
type Radikand   = Int

data StandardWerte = StandardWerte {
  radikands :: [Int],
  wurzelwerte_radikands :: [(Wurzelwert, Radikand)]
} deriving (Eq, Show)

-- Hauptfunktion welche die Schnittstelle difeniert
berechneWurzel :: Int -> Ergebnis
berechneWurzel radikand
  | multiplikator == 0 && rad == 0 = Ergebnis 0 0 False
  | einfacheWurzelwert == 0 = Ergebnis rad multiplikator True
  | otherwise = Ergebnis einfacheWurzelwert 0 True
    where standardWerte = berechneStandardwerte radikand
          einfacheErgebnis = berechneEinfacheWurzelwert radikand standardWerte
          komplexeWurzelwert = berechneKomplexeWurzelwert radikand standardWerte
          einfacheWurzelwert = ee_wurzelWert einfacheErgebnis
          multiplikator = ke_multiplikator komplexeWurzelwert
          rad = ke_radikand komplexeWurzelwert

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
  | null (radikands standardWerte) = EE 0
  | null einfacheWurzelWert          = EE 0
  | otherwise                        = EE (fst (head einfacheWurzelWert))
                                      where
                                        einfacheWurzelWert =
                                          filter
                                            (\radikand -> radikand_ == snd radikand)
                                            (wurzelwerte_radikands standardWerte)

-- quotRem -> (It returns a tuple: (result of integer division, reminder) )
berechneKomplexeWurzelwert :: Int -> StandardWerte -> KomplexeErgebnis
berechneKomplexeWurzelwert radikand_ standardWerte
  | null wW    = KE 0 0
  | otherwise  = KE (snd $ radicand wW) (fst $ radicand wW)
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

