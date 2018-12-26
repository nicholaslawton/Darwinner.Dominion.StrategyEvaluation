module CardOrder (arbitraryCardOrder) where

import Card

arbitraryCardOrder :: Card -> Int
arbitraryCardOrder Province = 1
arbitraryCardOrder Duchy = 2
arbitraryCardOrder Estate = 3
arbitraryCardOrder Gold = 4
arbitraryCardOrder Silver = 5
arbitraryCardOrder Copper = 6
