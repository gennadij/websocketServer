module QuadEquation () where
import qualified ExactRoot as ER (berechneWurzel, Ergebnis ( .. ))

data Form = F {
  f_a :: Int,
  f_b :: Int,
  f_c :: Int
}

data Ergebnis = E {
  e_radikand :: Int,
  e_multiplikator :: Int
}

data ErgebnisDiskriminante = ED {
  ed_diskriminante :: Deskriminante,
  ed_anzahlNullstellen :: Int 
}

data Deskriminante = D {
  d_wurzelWert :: Int,
  d_multiplikator :: Int
}

berechne :: Form -> Ergebnis
berechne form = undefined

berechneDiskriminante :: Form -> ErgebnisDiskriminante
berechneDiskriminante f = ED diskriminante nullstellen
  where diskriminante :: Deskriminante 
        diskriminante = D (ER.wurzelWert (berechneRadikandFuerDeskriminate f)) (ER.multiplikator (berechneRadikandFuerDeskriminate f)) 
        nullstellen :: Int
        nullstellen = 1

berechneRadikandFuerDeskriminate :: Form -> ER.Ergebnis
berechneRadikandFuerDeskriminate f = ER.berechneWurzel berechneRadikand
  where berechneRadikand :: Int 
        berechneRadikand = (b * b) - (4 * a * c)
          where b = f_b f
                a = f_a f
                c = f_c f