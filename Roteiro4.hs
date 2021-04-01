geraA = [5,4..1]

geraB = ['a','c','e']

geraC = [1,4..16]

geraD1 = [1,(-2)..(-11)]
geraD2 = [1,5..17]
geraD = zip geraD1 geraD2

intervalo :: Int -> Int -> [Int]
intervalo a b | (a > b) = []
              | (a == b) = [a]
              | otherwise = [a,a+1..b]

par :: Int -> Bool
par x = if (mod x 2 == 0) then True else False

interpar :: Int -> Int -> [Int]
interpar a b | (a >= b) = []
             | (par a == True) && (par b == True) = [a+2,a+4..b-2]
             | (par a == True) && (par b == False) = [a+2,a+4..b-1]
             | (par a == False) && (par b == True) = [a+1,a+3..b-2]
             | (par a == False) && (par b == False) = [a+1,a+3..b-1]
 

quadrados :: Int -> Int -> [Int]
quadrados a b = [ x^2 | x <- [a,a+1..b] ]

seleciona_impares :: [Int] -> [Int]
seleciona_impares xs = [ x | x <- xs ,  par x == False]

tabuada :: Int -> [Int]
tabuada a = [ x | x <- [ a*1, a*2..a*10 ] ]

bissexto :: Int -> Bool
bissexto x = if (( not(mod x 100 == 0) && mod x 4 == 0 ) || (mod x 400 == 0)) then True else False

bissextos :: [Int] -> [Int]
bissextos xs = [ x | x <- xs, bissexto x == True ]

sublistas :: [[Int]] -> [Int]
sublistas [xs] = [ x | x <- xs ]









