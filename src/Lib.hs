module Lib
    ( 
    someFunc, 
    someFunc2
    ) where

import Funcoes
import Objetos

someFunc :: IO ()
someFunc = putStrLn "someFunc"

someFunc2 :: IO Jogo
someFunc2 = do
  let jogo = criaJogo "1234567894567891237891234562143.589736589721489721436553164297864297853197853164."
  loop jogo
