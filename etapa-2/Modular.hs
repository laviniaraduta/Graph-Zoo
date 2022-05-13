module Modular where

import Data.List
import Data.Function (on)
import qualified Data.Set as S
import StandardGraph

type Graph a = StandardGraph a

{-
    O partiție este o mulțime de submulțimi ale unei alte mulțimi, disjuncte
    (fără elemente comune) și care împreună conțin toate elementele originale.
    
    De exemplu, pentru mulțimea [1,2,3], o posibilă partiție este [[1], [2,3]].

    Va fi folosită în etapa 3.
-}
type Partition a = S.Set (S.Set a)

{-
    *** TODO ***

    Aplică o funcție pe fiecare element al unei liste, însă doar pe unul singur
    la un moment dat, păstrându-le pe celalte nemodificate. Prin urmare, pentru
    fiecare element din lista inițială rezultă câte o listă în lista finală,
    aferentă modificării doar a acelui element.

    Exemplu:

    > mapSingle (+10) [1,2,3]
    [[11,2,3],[1,12,3],[1,2,13]]
-}

-- trebuie sa impart multimea in 2 parti si sa pun elementul pe care se aplica f intre ele
-- pe rand, fiecare element din lista initiala se transforma o noua lista
-- ficare noue lista este formata astfel:
    -- iau primele n elemente
    -- pun lista formata din elementul curent pe care aplic functia
    -- adaug restul elementelor
-- n este de fapt lungimea acumulatorului, pentru pt a aplica f pe al-n lea element inseamna
-- ca am format deja primele n -1 liste
mapSingle :: (a -> a) -> [a] -> [[a]]
mapSingle f xs = reverse $ foldl (\acc x -> ((firtsPart (1 + length acc) xs) ++ [(f x)] ++ (secondPart (1 + length acc) xs)) : acc) [] xs
                    where
                        -- extrage lista cu primele n elemente din xs
                        firtsPart n xs = if (null (fst (splitAt n xs)))
                            then []
                            else init (fst (splitAt n xs))
                        -- extrage lista cu ultimele n elemente din xs
                        secondPart n xs = if (null (snd (splitAt n xs)))
                            then []
                            else (snd (splitAt n xs)) 
{-
    *** TODO ***

    Determină lista tuturor partițiilor unei liste. Deși mai sus tipul
    Partition a este definit utilizând mulțimi, aici utilizăm liste,
    pentru simplitate.

    Dacă vi se pare greu de urmărit tipul întors de funcție, cu 3 niveluri
    ale constructorului de tip listă, gândiți-vă așa:
    - avem nevoie de un nivel pentru o submulțime
    - încă un nivel pentru o partiție, care este o mulțime de submulțimi
    - încă un nivel pentru mulțimea tuturor partițiilor.

    Hint: Folosiți list comprehensions pentru a răspunde la întrebarea:
    dacă am obținut o partiție a restului listei, cum obținem o partiție
    a întregii liste, care include capul? (folosiți și mapSingle)

    Exemple:

    > partitions [2,3]
    [[[2],[3]],[[2,3]]]

    > partitions [1,2,3]
    [[[1],[2],[3]],[[1,2],[3]],[[2],[1,3]],[[1],[2,3]],[[1,2,3]]]
-}

-- partitia pentru lista noua se formeaza din 2 partitii
-- - adaugand la fiecare element din partitiile vechi 
-- - adaugand elementul singur intr-o lista la toate partitiile vechi
partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x : xs) = from_tail ++ new_partitions
        where
            old_partitions = partitions xs
            from_tail = [ys | zs <- old_partitions, ys <- mapSingle (x :) zs]
            new_partitions = [[x] : p | p <- old_partitions]
  
