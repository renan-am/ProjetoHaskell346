
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

sqDiff a b = ((read a :: Float)-(read b :: Float))**2

dist (x:a) (y:b) = (x,y,foldl1 (+) (zipWith sqDiff a b))

getDist a b = [dist x y | x <- a, y <- b]

addLabel a b = addLabel' a b []
  where 
    addLabel' [] _ res = res
    addLabel' ((y,x):xs) ((a,b)) res
      | (check' x b) == True = res ++ ((y,(a:x)):xs)
      | otherwise = addLabel' xs (a,b) ((y,x):res)

compara (a,b,c) (x,y,z) = if c < z then
  (a,b,c)
  else
  (x,y,z)

checkLabel x = foldl1 (compara) x


main = do 
  txt <- getContents
  let dat = prepare txt
  
  let classes = getClass dat 
  let coord = getCoord dat

  print dat
  print classes
  print coord

  let noLabel = [x | x <- coord, not(check classes x)]
  print noLabel

  let label = [x | x <- coord, (check classes x)]
  print label

  let aux = check classes ["aab"]
  print aux

  let aux2 = getDist noLabel label
  print aux2

  let aux3 = addLabel classes ("ez34","aa")
  print aux3

  let tst = zipWith sqDiff ["5","4","2"] ["1","7","3"]
  print tst

  let aux4 = checkLabel aux2
  print aux4