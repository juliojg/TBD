import Data.Set
import System.Environment


-- Cierre de conjuntos de dependencias funcionales

ccdf :: Set Char -> Set (Set Char, Set Char) -> Set (Set Char, Set Char)
ccdf r f = let pr   = powerSet r
               f'   = union f (reflex pr) --ME FALTABA UNIR ESTA F, AHORA ANDAAA
           in    ccdf' r f'
               
ccdf' :: Set Char -> Set (Set Char, Set Char) -> Set (Set Char, Set Char)
ccdf' r f'  = if (rec == f') then f' else ccdf' r rec
                   where rec = (let f'' = aument r f'
                                    f''' = transRec f'' f''
                               in f''') 


-- Cierre de un conjunto de atributos
ccda :: Set Char -> Set (Set Char, Set Char) -> Set Char
ccda a f = let f' = ccdf a f
           in Data.Set.foldl union empty (Data.Set.map (\x -> ccda' x f') (powerSet a))

ccda' :: Set Char -> Set (Set Char, Set Char) -> Set Char
ccda' x f = Data.Set.foldl union empty (Data.Set.map (\(y1,y2) -> if (y1 == x) then y2 else x) f)
-- En caso de que la condicion sea falsa, no deberia devolver nada. Pero es necesario devolver algo. Pongo x porque seguro va a estar en el resultado. Ademas, con la union se eliminan las repeticiones.
-- estamos comparando mal

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
aumentSet' s (x, y) = (union s x, union s y) -- else (x, y) -- not (isSubsetOf s x || isSubsetOf s x || isSubsetOf x s || isSubsetOf x s)
-- if (Data.Set.size (intersection s x) == 0 && Data.Set.size (intersection s y) == 0) then 

-- Regla de transitividad total (aplica hasta que no existen mas transitividades posibles)
transRec :: Set (Set Char, Set Char) -> Set (Set Char, Set Char) -> Set (Set Char, Set Char) 
transRec ss1 ss2 = let rec = trans ss1 ss2  
                    in if (rec == ss1) then ss1 else transRec rec ss2 

--Regla de transitividad entre dos relaciones (aplicada solo una vez)
trans :: Set (Set Char, Set Char) -> Set (Set Char, Set Char) -> Set (Set Char, Set Char)
trans ss1 ss2 = Data.Set.foldl union empty (Data.Set.map (\x -> trans' x ss2) ss1)

--Devuelve, si es que la hay, la relacion de transitividad entre dos dependencias 
transSet :: (Set Char, Set Char) -> (Set Char, Set Char) -> (Set Char, Set Char)
transSet (x1, y1) (x2, y2) = if y1 == x2 then (x1, y2) else (x2, y2)

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


r1 :: Set Char
r1 = fromList ['A','B','C','D']

f1 :: Set (Set Char, Set Char)
f1 = fromList [(fromList ['A'], fromList ['B']), (fromList ['C','B'], fromList ['A']),(fromList ['B'], fromList ['A','D'])]

r2 :: Set Char
r2 = fromList ['A','B','C','D', 'E', 'F']

f2 :: Set (Set Char, Set Char)
f2 = fromList [(fromList ['A','B'], fromList ['C']), (fromList ['B','D'], fromList ['E', 'F'])]

r3 :: Set Char
r3 = fromList ['A' .. 'J']

f3 :: Set (Set Char, Set Char)
f3 = fromList [(fromList ['A'], fromList ['I']), (fromList ['A','B'], fromList ['C']), (fromList ['A','D'], fromList ['G', 'H']), (fromList ['B','D'], fromList ['E', 'F']), (fromList ['H'], fromList ['J'])]

r4 :: Set Char
r4 = fromList ['A' .. 'H']

f4 :: Set (Set Char, Set Char)
f4 = fromList [(fromList ['A'], fromList ['B', 'C']), (fromList ['C'], fromList ['D']), (fromList ['D'], fromList ['G']), (fromList ['E'], fromList ['A']), (fromList ['E'], fromList ['H']), (fromList ['H'], fromList ['E'])]

r5 :: Set Char
r5 = fromList ['A' .. 'G']

f5 :: Set (Set Char, Set Char)
f5 = fromList [(fromList ['A'], fromList ['F']), (fromList ['A'], fromList ['G']), (fromList ['B'], fromList ['E']), (fromList ['C'], fromList ['D']), (fromList ['D'], fromList ['B']), (fromList ['E'], fromList ['A']), (fromList ['F','G'], fromList ['C'])]



