module Funcoes (
  criaJogo, 
  loop
               )  where

import Objetos
import Data.Char(digitToInt)


--Funcoes para verificar se um numero é seguro
linhaSegura :: Casa -> Tabuleiro -> Bool
linhaSegura _ []     = True
linhaSegura casa (x:xs)
    | fst (pos casa) == fst (pos x) && num casa == num x = False
    | otherwise                                          = linhaSegura casa xs

colunaSegura :: Casa -> Tabuleiro -> Bool
colunaSegura _ []     = True
colunaSegura casa (x:xs)
    | snd (pos casa) == snd (pos x) && num casa == num x = False
    | otherwise                                          = colunaSegura casa xs


quadranteSeguro :: Casa -> Tabuleiro -> Bool
quadranteSeguro _ []     = True
quadranteSeguro casa (x:xs)
    | quadrante casa == quadrante x && num casa == num x = False
    | otherwise                                          = quadranteSeguro casa xs


numSeguro :: Casa -> Tabuleiro -> Bool
numSeguro casa tab = linhaSegura casa tab && colunaSegura casa tab && quadranteSeguro casa tab

jogoSeguro :: Tabuleiro -> Bool
jogoSeguro [] = True
jogoSeguro (x:xs)
    | not (numSeguro x xs) = False
    | otherwise                = jogoSeguro xs

determinaQuadrante :: (Int, Int) -> Int
determinaQuadrante (x,y)
    | x < 4 && y < 4   = 1
    | x < 4 && y < 7   = 2
    | x < 4 && y < 10  = 3
    | x < 7 && y < 4   = 4
    | x < 7 && y < 7   = 5
    | x < 7 && y < 10  = 6
    | x < 10 && y < 4  = 7
    | x < 10 && y < 7  = 8
    | otherwise        = 9

--essa funcao determina a linha de uma casa baseada na sua posição na string de entrada
--função para ser usada junto com a criaJogo
determinaLinha :: Int -> Int
determinaLinha x
    | x > 72 = 1
    | x > 63 = 2
    | x > 54 = 3
    | x > 45 = 4
    | x > 36 = 5
    | x > 27 = 6
    | x > 18 = 7
    | x > 9  = 8
    | otherwise     = 9

--essa funcao determina a coluna de uma casa baseada na sua posição na string de entrada
--função para ser usada junto com a criaJogo
determinaColuna :: Int -> Int
determinaColuna y
    | y `mod` 9 == 0  = 1
    | otherwise       = 10 - (y `mod` 9)

--função para ser usada com um map. Cria casas incompletas, que devem ser ajsutadas mais tarde
criaCasa :: Char -> Casa
criaCasa c
   | c <= '9' && c > '0' = Casa (digitToInt c) False (0,0) 0
   | otherwise           = Casa 0 True (0,0) 0

--essa função determina as coordenadas e o quadrante de cada casa, e retorna um tabuleiro completo
completaTabuleiro :: Tabuleiro -> Tabuleiro
completaTabuleiro []   = []
completaTabuleiro (z:zs) = Casa (num z) (alteravel z) (x,y) quadrante : completaTabuleiro zs
    where
        tab = z:zs
        x = determinaLinha (length tab)
        y = determinaColuna (length tab)
        quadrante = determinaQuadrante (x,y)

--essa função irá criar um jogo a partir de uma string de 81 caracteres
--números escondidos devem ser representados por um ponto, traco ou qualquer caractere nao numerico
criaJogo :: String -> Jogo
criaJogo s = Jogo (completaTabuleiro (map criaCasa s)) False

inputToNum :: String -> Int
inputToNum s = read (head (words s))

inputToCoord :: String -> (Int, Int)
inputToCoord s = (x, y)
    where
        inpt = words s
        x = read (inpt !! 1)
        y = read (inpt !! 2)

inputValido :: Int -> (Int, Int) -> Bool
inputValido num coord
    |num < 1    = False
    |num > 9    = False
    |fst coord < 1 = False
    |fst coord > 9 = False
    |snd coord < 1 = False
    |snd coord > 9 = False
    |otherwise       = True

localizaCasa :: (Int, Int) -> Tabuleiro -> Casa
localizaCasa _ []  = Casa 0 False (0,0) 0 --Serve apenas para o haskell aceitar rodar essa função
localizaCasa coord (x:xs)
    | coord == pos x  = x
    | otherwise         = localizaCasa coord xs

verificaFim :: Tabuleiro -> Bool
verificaFim []        = True
verificaFim (x:xs)
    | num x == 0 = False
    | otherwise    = verificaFim xs

alteraCasa :: Casa -> Int -> Tabuleiro -> Tabuleiro
alteraCasa _ _ []           = []
alteraCasa casa num (x:xs)
    | pos x == pos casa = Casa num (alteravel x) (pos x) (quadrante x) : alteraCasa casa num xs
    | otherwise             = x : alteraCasa casa num xs

loop :: Jogo -> IO Jogo
loop j  = do
    putStr (show j)
    --lembro como o usuário deve usar o nosso app
    putStrLn "Digite seu movimento no formato 'num x y' (e.g.: 3 2 5)"
    i <- getLine
    let num = inputToNum i
    let coord = inputToCoord i
    let casa = localizaCasa coord (tabuleiro j)
    if not (inputValido num coord)
        then do
            putStrLn "input invalido! Digite apenas numeros válidos!"
            loop j
        else if not (alteravel casa)
            then do
                putStrLn "Você não pode alterar essa casa!"
                loop j
            else do --se eu puder alterar a casa, eu crio um novo estado do jogo
                let jR = Jogo (alteraCasa casa num (tabuleiro j)) False
                if verificaFim (tabuleiro jR) --se o jogo chegou no fim, eu vejo se o cara ganhou ou não
                    then do
                        if jogoSeguro (tabuleiro jR)
                            then do
                                putStrLn "Parece tudo correto pra mim!! Parabéns"
                                return jR
                            else do --aqui eu cancelo o ultimo movimento pro jogador achar o erro
                                putStrLn "Esse tabuleiro não parece correto!"
                                putStrLn "Há algum conflito de numeros! Tente corrigi-lo."
                                loop j
                        else loop jR
