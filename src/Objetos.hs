module Objetos where

data Jogo = Jogo { tabuleiro :: Tabuleiro
                , fim :: Bool}

type Tabuleiro = [Casa]

data Casa = Casa { num :: Int
                , alteravel :: Bool
                , pos :: (Int, Int)
                , quadrante :: Int
                }

instance Show Casa where
    show casa
        | num casa == 0 = "   "
        | alteravel casa  = "(" ++ show (num casa) ++ ")"
        | otherwise       = " " ++ show (num casa) ++ " "

instance Show Jogo where
   show jogo = "    1   2   3   4   5   6   7   8   9" ++ "\n"
    ++ "  ╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗" ++ "\n"

    ++ "1 ║" ++ show (head tab) ++ "│" ++ show (tab !! 1) ++ "│" ++ show (tab !! 2) ++ "║"
    ++ show (tab !! 3) ++ "│" ++ show (tab !! 4) ++ "│" ++ show (tab !! 5) ++ "║"
    ++ show (tab !! 6) ++ "│" ++ show (tab !! 7) ++ "│" ++ show (tab !! 8) ++ "║" ++ "\n"
    ++ "  ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢\n"

    ++ "2 ║" ++ show (tab !! 9) ++ "│" ++ show (tab !! 10) ++ "│" ++ show (tab !! 11) ++ "║"
    ++ show (tab !! 12) ++ "│" ++ show (tab !! 13) ++ "│" ++ show (tab !! 14) ++ "║"
    ++ show (tab !! 15) ++ "│" ++ show (tab !! 16) ++ "│" ++ show (tab !! 17) ++ "║" ++ "\n"
    ++ "  ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢\n"

    ++ "3 ║" ++ show (tab !! 18) ++ "│" ++ show (tab !! 19) ++ "│" ++ show (tab !! 20) ++ "║"
    ++ show (tab !! 21) ++ "│" ++ show (tab !! 22) ++ "│" ++ show (tab !! 23) ++ "║"
    ++ show (tab !! 24) ++ "│" ++ show (tab !! 25) ++ "│" ++ show (tab !! 26) ++ "║" ++ "\n"
    ++ "  ╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣\n"

    ++ "4 ║" ++ show (tab !! 27) ++ "│" ++ show (tab !! 28) ++ "│" ++ show (tab !! 29) ++ "║"
    ++ show (tab !! 30) ++ "│" ++ show (tab !! 31) ++ "│" ++ show (tab !! 32) ++ "║"
    ++ show (tab !! 33) ++ "│" ++ show (tab !! 34) ++ "│" ++ show (tab !! 35) ++ "║" ++ "\n"
    ++ "  ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢\n"

    ++ "5 ║" ++ show (tab !! 36) ++ "│" ++ show (tab !! 37) ++ "│" ++ show (tab !! 38) ++ "║"
    ++ show (tab !! 39) ++ "│" ++ show (tab !! 40) ++ "│" ++ show (tab !! 41) ++ "║"
    ++ show (tab !! 42) ++ "│" ++ show (tab !! 43) ++ "│" ++ show (tab !! 44) ++ "║" ++ "\n"
    ++ "  ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢\n"

    ++ "6 ║" ++ show (tab !! 45) ++ "│" ++ show (tab !! 46) ++ "│" ++ show (tab !! 47) ++ "║"
    ++ show (tab !! 48) ++ "│" ++ show (tab !! 49) ++ "│" ++ show (tab !! 50) ++ "║"
    ++ show (tab !! 51) ++ "│" ++ show (tab !! 52) ++ "│" ++ show (tab !! 53) ++ "║" ++ "\n"
    ++ "  ╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣\n"

    ++ "7 ║" ++ show (tab !! 54) ++ "│" ++ show (tab !! 55) ++ "│" ++ show (tab !! 56) ++ "║"
    ++ show (tab !! 57) ++ "│" ++ show (tab !! 58) ++ "│" ++ show (tab !! 59) ++ "║"
    ++ show (tab !! 60) ++ "│" ++ show (tab !! 61) ++ "│" ++ show (tab !! 62) ++ "║" ++ "\n"
    ++ "  ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢\n"

    ++ "8 ║" ++ show (tab !! 63) ++ "│" ++ show (tab !! 64) ++ "│" ++ show (tab !! 65) ++ "║"
    ++ show (tab !! 66) ++ "│" ++ show (tab !! 67) ++ "│" ++ show (tab !! 68) ++ "║"
    ++ show (tab !! 69) ++ "│" ++ show (tab !! 70) ++ "│" ++ show (tab !! 71) ++ "║" ++ "\n"
    ++ "  ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢\n"

    ++ "9 ║" ++ show (tab !! 72) ++ "│" ++ show (tab !! 73) ++ "│" ++ show (tab !! 74) ++ "║"
    ++ show (tab !! 75) ++ "│" ++ show (tab !! 76) ++ "│" ++ show (tab !! 77) ++ "║"
    ++ show (tab !! 78) ++ "│" ++ show (tab !! 79) ++ "│" ++ show (tab !! 80) ++ "║" ++ "\n"
    ++ "  ╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝\n"
        where
            tab = tabuleiro jogo
