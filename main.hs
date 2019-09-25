{--Projeto 1 MC346 - Haskell
Nome: Adolf Pereira da Costa RA:164933
Nome: Renan Clarindo Amorim  RA:186454
--}


--Função que recebe o input e quebra cada linha em elementos de uma lista e
--posteriormente transforma cada palavra da linha em elementos de uma nova lista
prepare a = split (lines a)
  where
    split x = split' x []
      where
        split' [] res = res
        split' (x:xs) res = split' xs  (res ++ [(words x)])

--Função que cria tuplas para cada classe(label), constituídas de (label,[lista de pontos])
getClass [] = []
getClass (x:xs)
  | x == [] = getClass' xs []
  | otherwise = getClass xs
  where
    getClass' [] res = res
    getClass' ([a,b]:xs) res = getClass' xs ((read b :: Int,[a]):res)


--Função que separa as coordenas de cada ponto em uma nova lista
getCoord x = getCoord' x []
  where
    getCoord' [] res = res
    getCoord' (x:xs) res
      | x == [] = res
      | otherwise = getCoord' xs (x:res)

--Verifica se um ponto já está classificado com uma label
check [] _ = False
check ((_,x):xs) (a:_)
  | (check' x a) == True = True
  | otherwise = check xs [a]

check' [] _ = False
check' (x:xs) a
  | x == a = True
  | otherwise = check' xs a

{--
    a = lista de elementos sem Label (NoLabel)
    b = lista de elementos com Label (Label)
    Função que retorna uma lista de tuplas com o nome de dois pontos e a distância ao quadrado entre eles
--}
getDist a b = [dist x y | x <- a, y <- b]
  where
    dist (x:a) (y:b) = (x,y,foldl1 (+) (zipWith sqDiff a b))
      where
        sqDiff a b = ((read a :: Float)-(read b :: Float))**2

--Função que adiciona o nome do ponto dentro da lista de labels
addLabel a b = addLabel' a b []
  where 
    addLabel' [] _ res = res
    addLabel' ((y,x):xs) ((a,b,c)) res
      | (check' x b) == True = res ++ ((y,(a:x)):xs)
      | otherwise = addLabel' xs (a,b,c) ((y,x):res)

--Função que verifica qual o ponto de menor distância em relação a um label
--e retorna uma tupla com ponto, nome do label e a distância
checkLabel x = foldl1 (compara) x
  where 
    compara (a,b,c) (x,y,z) = if c < z then
      (a,b,c)
      else
      (x,y,z)

getLabel c cl = [x | x <- c, (check cl x)]
getNoLabel c cl = [x | x <- c, not(check cl x)]

-- Função que dado uma lista de pontos com suas coordenadas, e uma lista de pontos com label, classifica os pontos a partir do ponto mais próximo de um ponto com label
classify c cl = classify' c cl (getLabel c cl) (getNoLabel c cl)
  where 
    classify' _ cl _ [] = cl
    classify' c cl l nL = classify' c cl' (getLabel c cl') (getNoLabel c cl')
      where
        cl' = addLabel cl ((checkLabel  (getDist nL l )))



--Função que ordena as tuplas de acordo com o label de forma crescente
orderByGroup [] = []
orderByGroup (x:xs) = orderByGroup menores ++ [x] ++ orderByGroup maiores
  where  
    menores = [y | y <- xs, compara' y x]
    maiores = [y | y <- xs, not(compara' y x)]

    compara' (a,_) (b,_) = a <= b


--Função que ordena as tuplas de acordo com o os nomes dos labels de forma alfabética  
orderByElem [] = []
orderByElem ((a,b):xs) = ((a,(orderByElem' b)):orderByElem xs)

orderByElem' [] = []
orderByElem' (x:xs) = orderByElem' less ++ [x] ++ orderByElem' more
  where
    less = [y | y <- xs, y<=x]
    more = [y | y <- xs, y>x] 



main = do 
  txt <- getContents
  let dat = prepare txt
  let classes = getClass dat 
  let coord = getCoord dat  
  let res = orderByElem(orderByGroup (classify coord classes))

  mapM_ print (res)