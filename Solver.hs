module Solver where

import Formula
import ExtendedFormula

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

{-
Permite vizualizarea mai lizibilă a rezultatului funcției solve de mai jos,
având tipul (Maybe Interpretation, History).

>>> RV (Just $ Set.fromList [1, 2], historyExample)
Pure {getLiteral = -3} => []
Unit {getLiteral = -1, getClause = fromList [-1,4]} => [[-3,2]]
Decide {getLiteral = -4} => [[-3,1,2],[-1]]
NOP => [[-4,-2,3],[-3,1,2],[-1,4]]
-----
Just (fromList [1,2])
-}
newtype ResultVisualizer = RV (Maybe Interpretation, History)
instance Show ResultVisualizer where
    show (RV (mInterpretation, history)) =
        show (HV history) ++ "\n-----\n" ++ show mInterpretation

{-
*** TODO ***

Implementați funcția resolve, care rezolvă două clauze, pe baza literalului 
primit ca parametru, care apare garantat în prima clauză. Se disting următoarele
situații:

* Dacă complementul literalului nu apare în cea de-a doua clauză, funcția o 
  întoarce pe aceasta nemodificată.
* Dacă complementul literalului apare în cea de-a doua clauză, funcția întoarce
  reuniunea celor două clauze, din care se înlătură literalul și complementul 
  său.

Exemple:

>>> resolve 1 (toClause [1, 2]) (toClause [3])
fromList [3]

>>> resolve 1 (toClause [1, 2]) (toClause [1, 3])
fromList [1,3]

>>> resolve 1 (toClause [1, 2]) (toClause [-1, 3])
fromList [2,3]

>>> resolve (-1) (toClause [-1, 2]) (toClause [1, 3, 4])
fromList [2,3,4]
-}
resolve :: Literal -> Clause -> Clause -> Clause
resolve literal clause1 clause2 = 
    let negLiteral = complement literal
        clause1' = Set.delete literal clause1
        clause2' = Set.delete negLiteral clause2
    in if Set.member negLiteral clause2
       then Set.union clause1' clause2'
       else clause2

{-
*** TODO ***

Implementați funcția learn, care învață o nouă clauză în baza unei clauze 
curente și a unei liste de acțiuni în sens anticronologic, corespunzătoare unui
istoric din etapa 2. Se disting următoarele situații:

* Orice acțiune diferită de Unit este ignorată.
* O acțiune Unit impune rezolvarea clauzei originale stocate în acțiune cu 
  clauza curentă, în baza literalului stocat de asemenea în acțiune, producând
  o nouă clauză curentă.

CONSTRÂNGERI:

* Evitați recursivitatea explicită, valorificând funcționalele pe liste
  și funcțiile definite mai sus.
* Utilizați stilul point-free.
* Utilizați funcția resolve.

Exemple:

>>> learn (toClause [1, 2]) [Unit (-1) (toClause [-1, 3])]
fromList [2,3]

>>> learn (toClause [1, 2]) [Unit (-1) (toClause [-1, 3]), Unit (-2) (toClause [-2, 4])]
fromList [3,4]

>>> learn (toClause [1, 2]) [Pure 2, Decide 1]
fromList [1,2]
-}
learn :: Clause -> [Action] -> Clause
learn = foldl (\acc action -> case action of
  Unit literal c -> resolve literal c acc
  _ -> acc)

{-
*** TODO ***

Implementați funcția satisfy, care primește ca parametru o formulă simplă, ca 
în etapa 1, și încearcă să o satisfacă, întorcând o pereche cu o intepretare 
opțională, prezentă doar dacă formula este satisfiabilă, și istoricul curent.

Algoritmul de satisfacere este următorul:

1. Se prelucrează toate clauzele unitare (funcția processUnitClauses).
2. Dacă formula devine vidă, formula originală este satisfiabilă și se 
   construiește interpretarea utilizând istoricul curent. STOP.
3. Dacă formula conține clauza vidă (conflict), se învață o nouă clauză 
   (funcția learn).
   3a. Dacă clauza învățată este vidă, formula este nesatisfiabilă. STOP.
   3b. Altfel, se revine în istoric la cel mai distant punct în care clauza
       învățată este unitară (funcția backtrackToUnitClause), și se sare la 
       pasul 1.
4. Se prelucrează toți literalii puri (funcția processPureLiterals) și se sare 
   la pasul 1.
5. Numai dacă nu există literali puri, se asumă cel mai mic literal (funcția 
   decide) și se sare la pasul 1.

CONSTRÂNGERI:

* Utilizați gărzi și pattern guards (vedeți descrierea laboratorului 6).

Exemple:

>>> RV $ satisfy $ toFormula [[1, 2], [-1]]
Unit {getLiteral = 2, getClause = fromList [1,2]} => []
Unit {getLiteral = -1, getClause = fromList [-1]} => [[2]]
NOP => [[-1],[1,2]]
-----
Just (fromList [-1,2])

Mai sus, două acțiuni Unit satisfac formula, iar interpretarea este { -1, 2}.

>>> RV $ satisfy $ toFormula [[1, 2, 3], [2, -3], [-1]]
Pure {getLiteral = 2} => []
Unit {getLiteral = -1, getClause = fromList [-1]} => [[-3,2],[2,3]]
NOP => [[-3,2],[-1],[1,2,3]]
-----
Just (fromList [-1,2])

Mai sus, existența clauzei unitare { -1} impune mai întâi acțiunea aferentă,
după care este posibilă eliminarea literalului pur 2, care satisface formula.

>>> RV $ satisfy formulaExample
Pure {getLiteral = -3} => []
Unit {getLiteral = -1, getClause = fromList [-1,4]} => [[-3,2]]
Decide {getLiteral = -4} => [[-3,1,2],[-1]]
NOP => [[-4,-2,3],[-3,1,2],[-1,4]]
-----
Just (fromList [-4,-3,-1])

Mai sus, este reluat exemplul din scheletul etapei 2, în care se evidențiază
aceeași secvență de acțiuni.

>>> RV $ satisfy $ toFormula [[-1, -2, 3], [-1, 4, -3], [-1, -4, 5], [-1, -5, -3], [1, 2, 4]]
Unit {getLiteral = 1, getClause = fromList [1,2,4]} => []
Decide {getLiteral = -2} => [[1]]
Decide {getLiteral = -3} => [[-2,-1],[1,2]]
Decide {getLiteral = -4} => [[-3,-1],[-2,-1,3],[1,2]]
Decide {getLiteral = -5} => [[-4,-1],[-3,-1,4],[-2,-1,3],[1,2,4]]
NOP => [[-5,-3,-1],[-4,-1,5],[-3,-1,4],[-2,-1,3],[1,2,4]]
-----
Just (fromList [-5,-4,-3,-2,1])

>>> RV $ satisfy $ toFormula [[1], [-1]]
Unit {getLiteral = -1, getClause = fromList [-1]} => [[]]
NOP => [[-1],[1]]
-----
Nothing

Mai sus, se învață clauza vidă, deci formula este nesatisfiabilă.

>>> RV $ satisfy $ toFormula [[-7, 1], [-5, 1], [-3, 4], [3, -4], [-1, 2], [1, 2], [5, 7, -2], [-6, 1, -2], [6, -1, -2]]
Unit {getLiteral = -3, getClause = fromList [-3,4]} => []
Decide {getLiteral = -4} => [[-3]]
Unit {getLiteral = 6, getClause = fromList [-2,-1,6]} => [[-4,3],[-3,4]]
Unit {getLiteral = 2, getClause = fromList [-1,2]} => [[-4,3],[-3,4],[6]]
Unit {getLiteral = 1, getClause = fromList [-5,1]} => [[-4,3],[-3,4],[-2,6],[2]]
Unit {getLiteral = 5, getClause = fromList [5,7]} => [[-6,-2,1],[-4,3],[-3,4],[-2,-1,6],[-1,2],[1],[1,2]]
Decide {getLiteral = -7} => [[-6,-2,1],[-5,1],[-4,3],[-3,4],[-2,-1,6],[-2,5],[-1,2],[1,2],[5]]
NOP => [[-7,1],[-6,-2,1],[-5,1],[-4,3],[-3,4],[-2,-1,6],[-2,5,7],[-1,2],[1,2],[5,7]]
-----
Just (fromList [-7,-4,-3,1,2,5,6])

Exemplul de mai sus este cel din enunț, în care se învață clauza {5, 7}. Pentru 
completitudine, mai jos este istoricul intermediar obținut exact înainte de 
backtracking, declanșat de obținerea unei clauze vide.

Unit {getLiteral = -1, getClause = fromList [-1,2]} => [[],[-4,3],[-3,4]]
Unit {getLiteral = -2, getClause = fromList [-2,5,7]} => [[-4,3],[-3,4],[-1],[1]]
Decide {getLiteral = -5} => [[-4,3],[-3,4],[-2],[-2,-1],[-1,2],[1,2]]
Decide {getLiteral = -6} => [[-5,1],[-4,3],[-3,4],[-2,-1],[-2,5],[-1,2],[1,2]]
Decide {getLiteral = -7} => [[-6,-2,1],[-5,1],[-4,3],[-3,4],[-2,-1,6],[-2,5],[-1,2],[1,2]]
NOP => [[-7,1],[-6,-2,1],[-5,1],[-4,3],[-3,4],[-2,-1,6],[-2,5,7],[-1,2],[1,2]]
-}
satisfy :: Formula -> (Maybe Interpretation, History)
satisfy formula = 
    let ef = extendFormula formula
    in satisfy' ef [] [(NOP, ef)]
  where
    satisfy' :: ExtendedFormula -> [Literal] -> History -> (Maybe Interpretation, History)
    satisfy' extendedFormula interpretation history
      | Just (literal, _) <- firstUnitClause extendedFormula =
          let newHistory = processUnitClauses history
              newExtendedFormula = snd (head newHistory)
          in satisfy' newExtendedFormula interpretation newHistory

      | Map.null extendedFormula = 
          let finalInterpretation = extractAllLiterals history
          in (Just (Set.fromList finalInterpretation), history)

      | any Set.null (Map.keys extendedFormula) =
          let learnedClause = learn Set.empty (map fst history)
          in if Set.null learnedClause
             then (Nothing, history)
             else let newHistory = backtrackToUnitClause learnedClause history
                  in satisfy' (snd (head newHistory)) interpretation newHistory

      | Just pureLiteral <- firstPureLiteral extendedFormula =
          let newHistory = processPureLiterals history
              newExtendedFormula = snd (head newHistory)
          in satisfy' newExtendedFormula interpretation newHistory

      | otherwise =
          let newHistory = decide history
              newExtendedFormula = snd (head newHistory)
          in satisfy' newExtendedFormula interpretation newHistory

    extractAllLiterals :: History -> [Literal]
    extractAllLiterals history = 
        [lit | (action, _) <- history, lit <- case action of
            Unit lit _ -> [lit]
            Pure lit -> [lit]
            Decide lit -> [lit]
            NOP -> []]

{-
Clasă ale cărei instanțe reprezintă probleme reductibile la SAT.

Clasa este parametrizată cu variabila de tip problem, și conține două funcții:

* encode transformă o instanță a problemei într-o instanță SAT, construind 
  formula corespunzătoare.
* decode transformă o interpretare în soluția problemei originale.

Variabila de tip problem referă o instanță a unei probleme, care conține
informații atât despre intrare, utilizată de encode și de decode, cât și despre 
ieșire, populată de decode. Prezența informațiilor despre ieșire în cadrul
aceleiași reprezentări care conține și informațiile despre intrare poate fi 
utilă, de exemplu, dacă se impune o soluție parțială încă de dinaintea 
codificării.
-}
class Reducible problem where
    encode :: problem -> Formula
    decode :: Interpretation -> problem -> problem

{-
Permite rezolvarea unei probleme prin reducere la și apoi de la SAT.
-}
reduceSolve :: Reducible problem => problem -> Maybe problem
reduceSolve problem = fmap (`decode` problem) $ fst $ satisfy $ encode problem

{-
Tipuri de date necesare reprezentării problemei 3-colorare.

* Node este tipul unui nod din graf.
* Graph este reprezentarea unui graf neorientat, ca mulțimi de noduri și de 
  muchii.
* Color denotă cele trei culori posibile.
* ThreeColoring este reprezentarea unei instanțe a problemei 3-colorare,
  în care câmpul graph desemnează intrarea, iar coloring, ieșirea. Cu toate
  că nu vom utiliza această facilitate în temă, câmpul coloring ar putea fi
  parțial populat încă de la început, înainte de reducerea la SAT, dacă se 
  dorește impunerea a priori a unor culori asupra anumitor noduri.
-}
type Node = Int

data Graph = Graph
    { nodes :: Set Node
    , edges :: Set (Node, Node)
    } deriving (Show, Eq)

data Color = Red | Green | Blue
    deriving (Show, Eq)

type Coloring = Map Node Color

data ThreeColoring = ThreeColoring
    { graph    :: Graph     -- intrarea
    , coloring :: Coloring  -- ieșirea
    } deriving (Show, Eq)

{-
*** TODO ***

Instanțiați clasa Reducible cu tipul ThreeColoring, implementând funcțiile
encode și decode, utilizând principiile din enunț.
-}
instance Reducible ThreeColoring where
    encode problem =
        let g = graph problem
            
            atLeastOneColorClauses = Set.map (\n -> toClause [10 * n + 1, 10 * n + 2, 10 * n + 3]) (nodes g)
            
            atMostOneColorClauses = Set.foldl (\acc n -> 
                Set.union acc (Set.fromList
                    [ toClause [-(10 * n + 1), -(10 * n + 2)]
                    , toClause [-(10 * n + 1), -(10 * n + 3)]
                    , toClause [-(10 * n + 2), -(10 * n + 3)]
                    ])) Set.empty (nodes g)
            
            differentColorsClauses = Set.foldl (\acc (n, o) -> 
                Set.union acc (Set.fromList
                    [ toClause [-(10 * n + 1), -(10 * o + 1)]
                    , toClause [-(10 * n + 2), -(10 * o + 2)]
                    , toClause [-(10 * n + 3), -(10 * o + 3)]
                    ])) Set.empty (edges g)
        in 
            Set.unions [atLeastOneColorClauses, atMostOneColorClauses, differentColorsClauses]
    
    decode interpretation problem = 
        let g = graph problem
            colorMap = Set.fold (\node acc -> 
                let color = if Set.member (10 * node + 1) interpretation 
                            then Red
                            else if Set.member (10 * node + 2) interpretation 
                                 then Green
                                 else Blue 
                in Map.insert node color acc
              ) Map.empty (nodes g)
        in problem { coloring = colorMap }
{-
Exemple de grafuri neorientate.

>>> reduceSolve $ ThreeColoring graph2 Map.empty
Just (ThreeColoring {graph = Graph {nodes = fromList [1,2], edges = fromList [(1,2)]}, coloring = fromList [(1,Green),(2,Red)]})

>>> reduceSolve $ ThreeColoring graph3 Map.empty
Just (ThreeColoring {graph = Graph {nodes = fromList [1,2,3], edges = fromList [(1,2),(1,3),(2,3)]}, coloring = fromList [(1,Blue),(2,Green),(3,Red)]})
-}
graph2 :: Graph
graph2 = Graph
    { nodes = Set.fromList [1, 2]
    , edges = Set.fromList [(1, 2)]
    }

graph3 :: Graph
graph3 = Graph
    { nodes = Set.fromList [1, 2, 3]
    , edges = Set.fromList [(1, 2), (1, 3), (2, 3)]
    }
