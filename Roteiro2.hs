dobro :: Int -> Int
dobro x = x * 2

quad :: Int -> Int
quad x = dobro(dobro(x))

hip :: Float -> Float -> Float
hip co ca = sqrt((co * co) + (ca * ca))

dist :: Float -> Float -> Float -> Float -> Float
dist xa ya xb yb = sqrt( ((xb - xa)*(xb - xa)) + ((yb - ya)*(yb - ya)) )

converter :: Float -> (Float,Float,Float)
converter x = (x, x*3.96, x*4.45)

bissexto :: Int -> Bool
bissexto x = if (( not(mod x 100 == 0) && mod x 4 == 0 ) || (mod x 400 == 0)) then True else False

type Data = (Int,Int,Int)
bissexto2 :: Data -> Bool
bissexto2 (x, y, z) = if (bissexto z == True) then True else False

valida :: Data -> Bool
valida(x, y, z) | ((y==1) || (y==3) || (y==5) || (y==7) || (y==8) || (y==10) || (y==12)) && (x>0 && x <=31) = True
                | ((y==4) || (y==6) || (y==8) || (y==9) || (y==11)) && (x>0 && x <=30) = True
                | ((y==2) && (bissexto z == True) && (x>0 && x<=29)) = True
                | otherwise = False

precede :: Data -> Data -> Bool
precede (d1,m1,a1) (d2,m2,a2) | (a1 < a2) = True
                              | ((a1 == a2) && (m1 < m2)) = True
                              | ((a1 == a2) && (m1 == m2) && (d1 < d2)) = True
                              | ((a1 == a2) && (m1 == m2) && (d1 == d2)) = False
                              | otherwise = False

type Livro = (String,String,String,String,Int)
type Aluno = (String,String,String,Int)
type Emprestimo = (String,String,Data,Data,String)

verif :: Emprestimo -> Data -> Bool
verif(s1,s2,(d1,m1,a1),(d2,m2,a2),z)(d3,m3,a3) | (precede(d2,m2,a2)(d3,m3,a3) == True) = False
                                     | otherwise = True
e1::Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")