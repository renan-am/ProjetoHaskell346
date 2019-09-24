
prepare a = split (lines a)
  where
    split x = split' x []
      where
        split' [] res = res
        split' (x:xs) res = split' xs  (res ++ [(words x)])

getClass [] = []
getClass (x:xs)
  | x == [] = getClass' xs []
  | otherwise = getClass xs
  where
    getClass' [] res = res
    getClass' ([a,b]:xs) res = getClass' xs ((read b :: Int,[a]):res)

getCoord x = getCoord' x []
  where
    getCoord' [] res = res
    getCoord' (x:xs) res
      | x == [] = res
      | otherwise = getCoord' xs (x:res)

getInt x = read x :: Int

check [] _ = False
check ((_,x):xs) (a:_)
  | (check' x a) == True = True
  | otherwise = check xs [a]

check' [] _ = False
check' (x:xs) a
  | x == a = True
  | otherwise = check' xs a


{--
    a = Elementos sem Label (NoLabel)
    b = Elementos com Label (Label)
--}
getDist a b = [dist x y | x <- a, y <- b]
  where
    dist (x:a) (y:b) = (x,y,foldl1 (+) (zipWith sqDiff a b))
      where
        sqDiff a b = ((read a :: Float)-(read b :: Float))**2


addLabel a b = addLabel' a b []
  where 
    addLabel' [] _ res = res
    addLabel' ((y,x):xs) ((a,b,c)) res
      | (check' x b) == True = res ++ ((y,(a:x)):xs)
      | otherwise = addLabel' xs (a,b,c) ((y,x):res)

compara (a,b,c) (x,y,z) = if c < z then
  (a,b,c)
  else
  (x,y,z)

checkLabel x = foldl1 (compara) x

getLabel c cl = [x | x <- c, (check cl x)]
getNoLabel c cl = [x | x <- c, not(check cl x)]

classify c cl = classify' c cl (getLabel c cl) (getNoLabel c cl)
  where 
    classify' _ cl _ [] = cl
    classify' c cl l nL = classify' c cl' (getLabel c cl') (getNoLabel c cl')
      where
        cl' = addLabel cl ((checkLabel  (getDist nL l )))




orderByGroup [] = []
orderByGroup (x:xs) = orderByGroup menores ++ [x] ++ orderByGroup maiores
  where  
    menores = [y | y <- xs, compara' y x]
    maiores = [y | y <- xs, not(compara' y x)]
        
compara' (a,_) (b,_) = a <= b 

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



  -- let aux = orderByElem res
  -- print aux

  -- let res2 =  addLabel classes ((checkLabel  (getDist (getNoLabel coord classes) (getLabel coord classes))))
  -- print res2