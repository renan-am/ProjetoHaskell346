data Group a b = Group Int b deriving (Show)  

add (Group a b) c = Group a (c:b)

create l a b = [(a, [b])]

inc ((a,x):xs) b = (a,b:x):xs

--test a b = foldl1 (+) (zipWith (**2) a b)

dist a b = (a-b)**2

main = do 
    

    let temp = Group 5 ["bh"]
    let temp2 = add temp "zx"

    let aux = create [] 5 "Hz"
    let aux2 = inc aux "Gb"

    print aux2

    --let aux3 = test vet1 vet2
    let aux3 = foldl1 (+) (zipWith dist vet1 vet2)
    print aux3
    --print aux2
