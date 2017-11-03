import Data.Set
import System.Environment





-- Regla de reflexividad, obtengo la reflexion de todos los sets, y luego las uno
reflex :: Set (Set Char) -> Set (Set Char, Set Char)
reflex s = Data.Set.foldl union empty (Data.Set.map (\x -> reflexSet x) s) 

reflexSet :: Set Char -> Set (Set Char, Set Char)
reflexSet s =  (Data.Set.map (\x -> (s,x)) ( delete empty (powerSet s)))

-- Regla de aumentatividad (agrega los elementos originales de la relacion, no deberia importar porque creo que se va a unir)
aument :: Set Char -> Set (Set Char, Set Char) ->  Set (Set Char, Set Char)
aument r f = Data.Set.foldl union empty (Data.Set.map (\x -> aumentSet r x) f) 

aumentSet :: Set Char -> (Set Char, Set Char) -> Set (Set Char, Set Char)
aumentSet r s = let p = powerSet r 
                in  (Data.Set.map (\x -> aumentSet' x s) p)  

--basta con que sean disjuntos???
aumentSet' :: Set Char -> (Set Char, Set Char) -> (Set Char, Set Char) 
aumentSet' s (x, y) = if (Data.Set.size (intersection s x) == 0 && Data.Set.size (intersection s y) == 0) then (union s x, union s y) else (x, y) -- not (isSubsetOf s x || isSubsetOf s x || isSubsetOf x s || isSubsetOf x s)


-- Regla de transitividad total (aplica hasta que no existen mas transitividades posibles)
transRec :: Set (Set Char, Set Char) -> Set (Set Char, Set Char) -> Set (Set Char, Set Char) 
transRec ss1 ss2 = let rec = trans ss1 ss2  
                    in if (rec == ss1) then ss1 else transRec rec ss2 

--Regla de transitividad entre dos relaciones (aplicada solo una vez)
trans :: Set (Set Char, Set Char) -> Set (Set Char, Set Char) -> Set (Set Char, Set Char)
trans ss1 ss2 = Data.Set.foldl union empty (Data.Set.map (\x -> trans' x ss2) ss1)

--Devuelve, si es que la hay, la relacion de transitividad entre dos dependencias 
transSet :: (Set Char, Set Char) -> (Set Char, Set Char) -> (Set Char, Set Char)
transSet (x1, y1) (x2, y2) = if y1 == x2 then (x1, y2) else (x1, y1)

--Devuelve todas las relaciones de transitividad entre una relacion y un conjunto de ellas
trans' :: (Set Char, Set Char) -> Set (Set Char, Set Char) -> Set (Set Char, Set Char)
trans' s ss = insert s (union (Data.Set.map (\x -> transSet s x) ss) ss)

 
--Ejemplo: aument (fromList ['1','2','3']) (fromList [(fromList ['1'], fromList ['2']) , (fromList ['2'], fromList ['3']) ])



-- Dado un conjunto, obtiene el conjunto de partes de dicho conjunto
powerSet :: Set Char -> Set (Set Char)
powerSet s = case Data.Set.null s of True  -> Data.Set.singleton empty
                                     False ->  union xss (Data.Set.map (Data.Set.insert (Data.Set.elemAt 0 s)) xss)
                                                   where xss = powerSet (Data.Set.deleteAt 0 s)

-- Implementacion original para listas
-- powerset :: [a] -> [[a]]
-- powerset [] = [[]]
-- powerset (x:xs) = xss ++ map (x:) xss
--                 where xss = powerset xs

