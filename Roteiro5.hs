conta_ch :: [Char] -> Int
conta_ch [] = 0
conta_ch (x:tail) = conta_ch tail + 1

conta :: [tipo] -> Int
conta [] = 0
conta (x:tail) = 1 + conta tail

maior :: [Int] -> Int
maior [x] = x
maior (x:y:tail) | ( x > y ) = maior(x:tail)
                 | otherwise = maior(y:tail)

primeiros :: Int -> [tipo] -> [tipo]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:tail) = x : primeiros (n-1) tail

pertence :: Eq tipo => tipo -> [tipo] -> Bool
pertence x [] = False
pertence x (h:tail) | (x == h) = True
                    | otherwise = pertence x tail

uniaoR :: Eq tipo => [tipo] -> [tipo] -> [tipo]
uniaoR [] lista = lista
uniaoR (h:tail) lista | ((pertence h lista) == True ) = uniaoR tail lista 
                      | otherwise = h : uniaoR tail lista

npares :: [Int] -> Int
npares [] = 0
npares (h:tail) | (mod h 2 == 0) =  npares tail + 1
                | otherwise = npares tail

produtorio :: [Int] -> Int
produtorio [] = 0
produtorio [x] = x
produtorio (h:tail) = h * produtorio tail

tamanho :: [tipo] -> Int
tamanho [] = 0
tamanho (h:tail) = 1 + tamanho tail
