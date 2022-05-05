{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))
-- Data StandardGraph a
--  = Empty 
--  | Pair S.set a S.set (a, a) 

-- Data StandardGraph a = Graph
--     { nodes :: S.set a
--     , edges :: S.set (a, a)
--     }

-- fromComponents [] [] = Empty
-- fromComponents ns es = 

{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = (S.fromList ns, S.fromList es)

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = fst

{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = snd

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = S.map snd $ S.filter ((== node) . fst) (edges graph)
-- outNeighbors node graph = S.map (\pair -> snd pair) (S.filter (\pair -> node == (fst pair)) (edges graph))

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = S.map fst $ S.filter ((== node) . snd) (edges graph)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph =
    if S.notMember node (fst graph)
        then
            graph
        else
            -- pastrez doar nodurile diferite de cel dat ca parametru
            -- pastrez doar muchiile care nu au nici pe prima pozitie nici pe a2a nodul eliminat 
            (S.filter (/= node) (nodes graph),
            S.filter (\pair -> ((fst pair) /= node) && ((snd pair) /= node)) (edges graph))
            -- S.difference (edges graph) (S.union (inNeighbors node graph) (outNeighbors node graph)))


{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])

    graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]
-}
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old news graph = (newNodes, newEdges) where
    -- pstrez doar nodurile diferite de old si le adau pe cee noi
    newNodes = S.fromList (S.toList (S.filter (/= old) (nodes graph)) ++ news)
    -- extrag nodurile care ies si care intra din cel vechi
    ins = inNeighbors old graph
    outs = outNeighbors old graph
    -- pastrez separat muchiile care nu au legatura cu cel vechi
    rest = S.filter (\pair -> ((fst pair) /= old) && ((snd pair) /= old)) (edges graph)
    -- muchiile noi, facute cu nodurile noi se obtin facand produsul cartezian intre:
    --          - nodurile noi si nodurile din care ieseau din vechiul nod
    --          - nodurile care intrau in cel vechi si nodurile noi
    --          - la care adau muchiile care n-au fost afectate de schimbare
    newEdges = S.fromList ((S.toList (S.cartesianProduct (S.fromList news) outs)) ++ (S.toList (S.cartesianProduct ins (S.fromList news))) ++ S.toList rest)
{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}
mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph =
    -- daca niciun nod nu are proprietatea ceruta, nu se schimba nimic
    if (S.filter (\x -> (prop x)) (nodes graph)) == S.empty
    then
        graph
    else 
        (newNodes, newEdges) where
            -- pastrez doar nodurile care nu respecta proprietatea si adaug nodul nou
            newNodes = S.fromList ((S.toList (S.filter (\x -> not (prop x)) (nodes graph))) ++ [node])
            nodesWithProp = S.filter prop (nodes graph) 
            -- inlocuiesc in fiecare pereche in care gasesc unul din nodurile cu proprietatea ceruta cu noul nod
            newEdges = S.map (\pair ->
                if S.member (fst pair) nodesWithProp && S.member (snd pair) nodesWithProp
                    then (node, node)
                else if S.member (fst pair) nodesWithProp
                    then (node, snd pair)
                else if S.member (snd pair) nodesWithProp
                    then (fst pair, node)
                else
                    pair) (edges graph)