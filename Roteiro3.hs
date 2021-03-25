dist :: Float -> Float -> Float -> Float -> Float
dist xa ya xb yb = sqrt( ((xb - xa)*(xb - xa)) + ((yb - ya)*(yb - ya)) )

fat1 :: Int -> Int
fat1 0 = 1
fat1 x = x * (fat1(x-1))

fat2 x | (x == 0) = 1
       | (x > 0) = x * (fat2(x-1))

fibo :: Int -> Int
fibo 1 = 0
fibo 2 = 1
fibo x = fibo(x-1) + fibo(x-2)

n_tri :: Int -> Int
n_tri 1 = 0
n_tri x = n_tri(x-1) + (x-1)

potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 1 = 2
potencia2 x = potencia2(x-1) * 2

prodIntervalo :: Int -> Int -> Int
prodIntervalo m n = if( n == m + 1) then m*n else m * (prodIntervalo(m+1) n)
fatorial :: Int -> Int
fatorial x = x * (prodIntervalo 1 (x-1))

--Pulando o 8--

mdc1 :: Int -> Int -> Int
mdc1 m n | (n == 0) = m
         | (n > 0) = mdc1 n (mod m n)

mdc2 :: Int -> Int -> Int
mdc2 m n = if(n == 0) then m else mdc2 n (mod m n)
