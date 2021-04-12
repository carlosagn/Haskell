--1--
analisa_raizes :: Float -> Float -> Float -> Float
analisa_raizes a b c | ( a == 0) = 4
                     | ( (b*b) > (4*a*c) ) = 1
                     | ( (b*b) == (4*a*c) ) = 2
                     | ( (b*b) < (4*a*c) ) = 3

--2--
equacao :: Float -> Float -> Float -> (Float,Float)
equacao a b c | ( a == 0 ) = ( (-c)/b, 0 )
              | ( a /= 0) = ( ( (-b) + sqrt( (b*b) - (4*a*c) ) ) / (2*a) , ( (-b) - sqrt( (b*b) - (4*a*c) ) ) / (2*a) )


--3--
type Data = (Int,Int,Int)

passagem :: Float -> Data -> Data -> Float
passagem x (d1,m1,a1) (d2,m2,a2) | ( abs(a2 - a1) < 2 ) = (15 * x)/100
                                 | ( abs(a2 - a1) <= 10 ) = (40 * x)/100
                                 | ( abs(a2 - a1) > 70 ) = (50 * x)/100
                                 | otherwise = x

--4--
gera1 :: [Int] 
gera1  = [ x*x*x | x <- [1,2..20], (mod x 2 == 0) && (x > 3) && (x < 11) ]

gera2 :: [(Int,Int)]
gera2 = [ (x,y) | x <- [1,2..5], y <- [1,2..20], (y < 20) && (y <= (3*x) )]

aux1 :: [Int] 
aux1 = [ a | a <- [1..15] ]
aux2 :: [Int]
aux2 = [ b | b <- [1..16] ]
gera3 :: [Int]
gera3 = (aux1) ++ (aux2)

gera4 :: [(Int,Int)]
gera4 = [ (x,y) | x <- [1..10], y <- [1..10], (mod x 2 == 0) && ( y == x + 1) ]

gera5 :: [Int]
gera5 = [ x + y | (x,y) <- gera4 ]

--5a--
tamanho :: [tipo] -> Int
tamanho [] = 0
tamanho (h:tail) = 1 + tamanho tail

contaNegM2 :: [Int] -> Int
contaNegM2 xs = tamanho[ x | x <- xs , ( x >= 0) && (mod x 3 == 0) ]

--5b--
listaNegM2 :: [Int] -> [Int]
listaNegM2 xs = [ x | x <- xs , ( x >= 0) && (mod x 3 == 0) ]

--6--
fatores :: Int -> [Int]
fatores n = [ x | x <- [1..n], mod n x == 0 ]

primos :: Int -> Int -> [Int]
primos a b = [ x | x <- [a..b], tamanho(fatores x) == 2 && x /= 2]

--7--
mdc :: Int -> Int -> Int
mdc x y | y > x = mdc y x
        | y == 0 = x
        | otherwise = mdc y (mod x y)

mmc1 :: Int -> Int -> Int
mmc1 x y = (x * y) `div` (mdc x y)

mmc :: Int -> Int -> Int -> Int
mmc x y z = mmc1 x (mmc1 y z)

--8--
serie :: Float -> Int -> Float
serie x n
    | (n == 1) = 1/x
    | even n = (x/fromIntegral(n)) + (serie x (n-1))
    | otherwise = (fromIntegral(n)/x) + (serie x (n-1))


--9--
fizzAux :: Int -> String
fizzAux x | (mod x 2 == 0) = "Fizz"
          | (mod x 3 == 0) = "Buzz"
          | (mod x 2 == 0) && ( mod x 3 == 0) = "FizzBuzz"
          | otherwise = "No"

fizzbuzz :: Int -> [String]
fizzbuzz n = [ fizzAux n | n <- [1..n] ]
  
--10--
sel_multiplos :: Int -> [Int] -> [Int]
sel_multiplos n lista = [ x | x <- lista, (mod x n == 0) ]

--11--
contador :: Int -> [Int] -> Int
contador _ [] = 0
contador n (h:tail) | (h == n) = contador n tail + 1
                    | otherwise = contador n tail

unica_ocorrencia :: Int -> [Int] -> Bool
unica_ocorrencia n lista | ( contador n lista == 1) = True
                         | otherwise = False

--12--
intercala :: Eq tipo => [tipo] -> [tipo] -> [tipo]
intercala [] lista = lista
intercala lista [] = lista
intercala (h:tail) lista2 = [h] ++ intercala lista2 tail

--13--
zipar :: Eq tipo => [tipo] -> [tipo] -> [[tipo]]
zipar [] lista = []
zipar lista [] = []
zipar (h:tail) (h2:tail2) = [[h,h2]] ++ zipar tail tail2 

--14--
type Contato = (String, String, String, String)

buscaNome :: String -> [Contato] -> String
buscaNome _ [] = "Email desconhecido"
buscaNome x ( (n, _, _, e):tail ) = if (x == e) then n else (buscaNome x tail)

--15--
type Pessoa = (String, Float, Int, Char)

pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66, 27,'F'),("João", 1.85, 26, 'C'),("Maria", 1.55, 62, 'S'),("Jose", 1.78, 42, 'C'),("Paulo", 1.93, 25, 'S'),("Clara", 1.70, 33, 'C'),("Bob", 1.45, 21, 'C'),("Rosana", 1.58,39, 'S'),("Daniel", 1.74, 72, 'S'),("Jocileide", 1.69, 18, 'S') ]

tamanho2 :: [Pessoa] -> Float
tamanho2 [] = 0
tamanho2 (h:tail) = 1 + (tamanho2 tail)

alturaTotal :: [Pessoa] -> Float
alturaTotal [] = 0
alturaTotal ( (_,altura,_,_):tail ) = altura + alturaTotal tail

alturaMedia :: [Pessoa] -> Float
alturaMedia lista = (alturaTotal lista) / tamanho2 lista

maisNova :: [Pessoa] -> Int
maisNova lista = minimum [ idade | (_,_,idade,_) <- lista ]

anos50 :: [Pessoa] -> [Pessoa]
anos50 [] = []
anos50 lista = [ (n,a,i,e) | (n,a,i,e) <- lista, i >= 50 ]

maisVelha :: [Pessoa] -> String
maisVelha [(n,_,_,e)] = n ++ "(" ++ [e] ++ ")"
maisVelha ( (n1,a1,i1,e1):(n2,a2,i2,e2):tail ) | i1 > i2 = maisVelha ((n1, a1, i1, e1):tail)
                                               | otherwise = maisVelha ((n2, a2, i2, e2):tail)

pessoasCasadas :: [Pessoa] -> Int
pessoasCasadas [] = 0
pessoasCasadas ( (_,_,i,e):tail ) | ( i > 35 ) && ( e == 'C' ) = pessoasCasadas tail + 1
                                  | otherwise = pessoasCasadas tail

--16--
insere_ord :: Ord tipo => tipo -> [tipo] -> [tipo]
insere_ord _ [] = []
insere_ord n (h:tail) | ( n > h ) = [h] ++ insere_ord n tail
                      | otherwise = [n] ++ (h:tail)

--17--
reverte :: Eq tipo => [tipo] -> [tipo]
reverte [] = []
reverte (h:tail) = reverte tail ++ [h]

--18--
pertence :: Eq tipo => tipo -> [tipo] -> Bool
pertence _ [] = False
pertence x (h:tail) | ( x == h ) = True
                    | otherwise = pertence x tail

elimina_repet :: Eq tipo => [tipo] -> [tipo]
elimina_repet [] = []
elimina_repet (h:tail) | ( pertence h tail == False ) = [h] ++ elimina_repet tail
                       | otherwise = elimina_repet tail

--19--
disponiveis :: [Int]
disponiveis = [1,2,5,10,20,50,100]

notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco x = [ h:tail | h <- disponiveis, x >= h, tail <- notasTroco (x-h) ]

--20--