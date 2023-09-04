module BinaryNumber where

import Data.List
import Data.Tuple (swap)

{-
    Reprezentarea unui număr binar ca listă finită sau infinită de biți.
    Primul bit este cel mai puțin semnificativ.

    De exemplu, 6 poate fi reprezentat ca [0, 1, 1, 0, 0, ...].

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type BinaryNumber = [Int]

{-
    *** TODO ***

    Transformă un număr din reprezentarea binară finită în cea zecimală.
    
    Constrângeri: funcția trebuie definită
    * în stil point-free (fără explicitarea parametrului formal)
    * utilizând o funcțională (fără recursivitate explicită).

    Exemple:
    
    > toDecimal [0,1,1]
    6
-}

toDecimal :: BinaryNumber -> Int
toDecimal = foldr (\x acc -> 2*acc + x) 0

{-

folosim foldr - elementele sunt prelucrate in ordinea dreapta-stanga (de la cel mai semnificativ bit)
acel (2*) pe care il aplicam asupra lui acc va creea puterea lui 2 corespunzatoare
exemplu:
pentru 0 1 1 0 1 = (2^4)*1 + (2^3)*0 + (2^2)*1 + (2^1)*1 + (2^0)*0
                2*0 + 1
             2*(2*0 + 1) + 0
          2*(2*(2*0 + 1) + 0) + 1
       2*(2*(2*(2*0 + 1) + 0) + 1) + 1
    2*(2*(2*(2*(2*0 + 1) + 0) + 1) + 1) + 0

-}


{-
    *** TODO ***

    Transformă un număr din reprezentarea zecimală în cea binară infinită.

    Constrângeri: pt bonus 10p, funcția trebuie definită
    * în stil point-free (fără explicitarea parametrului formal)
    * utilizând funcționala unfoldr (fără recursivitate explicită).

    Spre deosebire de foldr, care împăturește o listă la o singură valoare,
    unfoldr desfășoară procesul invers, construind o listă pe baza unei valori
    inițiale.
    
    Hint: divMod.

    Exemple:
    
    > take 10 $ toBinary 6
    [0,1,1,0,0,0,0,0,0,0]
-}

toBinary :: Int -> BinaryNumber
toBinary = unfoldr (\b -> Just (snd (divMod b 2), fst (divMod b 2)))

{-

vrem ca functia sa intoarca reprezentarea binara infinita a numarului 
functia unfoldr primeste o functie anonima care intoarce un Maybe
pentru a se opri unfoldr din procesarea numarului ar trebui sa punem o conditie de oprire in functia primita, 
caz in care sa returneze Nothing, deci omitem aceasta conditie si returnam la infinit Just (a, b)
unde a va fi adaugat la lista infinita cu reprezentarea binara a numarului (adica restul la impartirea cu 2)
unde b este urmatorul numar luat recursiv pe care e iar aplicata functia anonima (adica rezultatul impartirii la 2)
functia divMod intoarce o pereche cu rezultatul impartirii si restul

-}

{-
    *** TODO ***

    Incrementează cu 1 reprezentarea finită sau infinită a unui număr binar.
    Atenție la transport!

    Constrângeri: utilizați
    * recursivitate explicită
    * pattern matching.

    Exemple:

    > inc [0,1,1] 
    [1,1,1]

    > inc [1,1,1]
    [0,0,0,1]
-}

inc :: BinaryNumber -> BinaryNumber
inc [] = [1] -- daca se ajunge prin recursivitate la ultimul bit (cel mai semnificativ) pun bitul de transport
inc (1 : bits) =  0 : inc bits  -- daca dam de un 1, il incrementam si mergem recursiv si prin restul bitilor pentru ca trebuie sa retinem bitul de transport
inc (0 : bits) = 1 : bits -- daca dam de un 0, doar il incrementam si adaugam restul de biti

{-
   *** TODO ***

    Decrementează cu 1 reprezentarea finită sau infinită a unui număr binar.
    Atenție la împrumut!

    Constrângeri: utilizați
    * recursivitate explicită
    * pattern matching.

    Exemple:

    1 1 1
    1 1 0
    1 0 0 0
    1  1  1

    0 0 0 0
      1 1 1
    1  0  0  0


    > dec [1,1,1]
    [0,1,1]

    > dec [0,0,0,1]
    [1,1,1]
-}

dec :: BinaryNumber -> BinaryNumber
dec [1] = [] -- daca ultimul bit este 1, el va deveni 0 (cel mai semnificativ bit), deci nu il mai adaugam
dec [0] = 1 : dec (0 : [0]) -- daca ultimul este 0 (se ajunge aici doar cand avem biti de 0) -> vom returna un sir infinit de biti de 1
dec (1 : bits) = 0 : bits -- daca dam de un 1, doar il decrementam si adaugam restul bitilor
dec (0 : bits) = 1 : dec bits -- daca dam de un 0, decrementam bitul

{-
    *** TODO ***

    Adună două numere binare, asumând reprezentări infinite, pentru ușurința
    aplicării funcționalelor.

    Constrângeri: utilizați
    * where sau let
    * pt bonus 10p, funcționala mapAccumL (fără recursivitate explicită).

    mapAccumL are tipul (a -> b -> (a, c)) -> a -> [b] -> (a, [c]).
    Așa cum sugerează numele, combină comportamentele funcționalelor:
    * map, transformând element cu element [b] în [c]
    * foldl, utilizând în același timp un acumulator de tipul a.

    Exemple:

    > take 10 $ add (toBinary 74) (toBinary 123)
    [1,0,1,0,0,0,1,1,0,0]
    
    > toDecimal $ take 10 $ add (toBinary 74) (toBinary 123)
    197
-}

add :: BinaryNumber -> BinaryNumber -> BinaryNumber
add bits1 bits2 = snd $ mapAccumL addFunction 0 $ zip (inf bits1) (inf bits2)
        where   inf = \bits -> bits ++ (repeat 0) 
                addFunction = \acc (x, y) -> divMod (x + y + acc) 2

{-

- consideram reprezentarile in binar infinite (ne ajuta cand avem bit de transport), 
deci umplem cu 0 in continuarea bitului semnificativ
- functia addFunction primeste acc si pereche intre bitii de pe aceeasi pozitie
- functia mapAccumL va mapa fiecare pereche la suma bitilor din pereche + acc
- in acc vom retine bitul de transport

exemplu:
0 1 0 0 0 0 ...
1 1 0 0 0 0 ...

acc = 0 si (0, 1) => divMod (x + y + acc) 2 => divMod 1 2 = (0, 1) deci (0, 1) se mapeaza la 1 (rest)
                                                                        acc devine 0 (catul)
acc = 0 si (1, 1) => divMod (x + y + acc) 2 => divMod 2 2 = (1, 0) deci (1, 1) se mapeaza la 0 (rest)
                                                                        acc devine 1 (catul)
acc = 1 si (0, 0) => divMod (x + y + acc) 2 => divMod 1 2 = (0, 1) deci (0, 0) se mapeaza la 1 (rest)
                                                                        acc devine 0 (catul)
-}


{-
    *** TODO ***

    În pregătirea operației de înmulțire a două numere binare, cu reprezentări
    infinite, stivuiește copii deplasate la dreapta ale lui bits1, înmulțite
    cu bit-ul curent din bits2. Deplasarea se face adăugând la stânga lui bits1
    un număr de 0-uri dat de numărul liniei curente. Întoarce o listă infinită
    de liste infinite.

    Vizual:

    0 1 1 0 ... *   <- bits1
    1 0 1 0 ...     <- bits2
    -----------
   |0 1 1 0 ...        înmulțire bits1 cu 1 și deplasare 0 poziții la dreapta
    0|0 0 0 0 ...      înmulțire bits1 cu 0 și deplasare 1 poziție la dreapta
    0 0|0 1 1 0 ...    înmulțire bits1 cu 1 și deplasare 2 poziții la dreapta

    Constrângeri:
    * Corpul funcției trebuie să fie un list comprehension.
    * Nu utilizați recursivitate explicită.

    Hint: iterate pt generarea secvențelor de deplasare spre dreapta cu biți 0.

    Exemple:

    (exemplul vizual)
    > take 3 $ map (take 6) $ stack (toBinary 6) (toBinary 5)
    [[0,1,1,0,0,0],[0,0,0,0,0,0],[0,0,0,1,1,0]]
-}

stack :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
stack bits1 bits2 = [deplasare ++ (map (* bit) bits1) | n <- [0..], let bit = bits2 !! n, let deplasare = head $ take (n + 1) $ drop n $ iterate (0:) []]

{-

- trebuie sa returnam o lista de liste infinite
- deci aplic map pe bits1 -> fiecare bit din bits1 este mapat la el inmultit cu bitul din bits2 de pe indicele
corespunzator, retinut in generatorul n
- dupa calculez deplasarea cu 0 in functie de valoarea lui n pe care o adaug in fata sirului de biti mapat

-}


{-
    *** TODO ***

    Întoarce o listă infinită de numere binare, care pe poziția i >= 0 conține
    rezultatul înmulțirii lui bits1 cu numărul format din primii i biți
    ai lui bits2, i.e. suma primelor i linii întoarse de stack.

    Constrângeri:
    * Utilizați funcționala scanl (fără recursivitate explicită).

    Spre deosebire de foldl, care întoarce acumulatorul final, scanl întoarce
    o listă cu toate acumulatoarele intermediare.

    Exemple:
    
    > take 5 $ map (toDecimal . take 10) $ multiply (toBinary 6) (toBinary 5) 
    [0,6,6,30,30]
-}

multiply :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
multiply bits1 bits2 = scanl add (toBinary 0) (stack bits1 bits2)

{-

scanl va avea ca argumente functia add, acc/un sir infinit de biti de 0, lista infinita de liste infinite
calculate la ex anterior si calculez suma primelor i linii întoarse de stack, i >= 0

-}


{-
    *** TODO ***

    Întrebare de reflecție, la care veți răspunde la prezentarea temei.

    Având în vedere că liniile întoarse de stack care conțin exclusiv biți 0
    nu contribuie la rezultatul unei înmulțiri, să presupunem că modificăm
    definiția funcției stack astfel încât să construiască doar liniile utile;
    de exemplu, folosind filter sau pattern matching în list comprehension
    pt a păstra doar biții 1 din bits2. Ce probleme ar putea crea această
    abordare?

    -- daca pastram doar bitii de 1 din bits2 pierdem indexarea 
    -- deci nu am mai sti cu cat sa deplasam spre dreapta cu biti de 0
-}
