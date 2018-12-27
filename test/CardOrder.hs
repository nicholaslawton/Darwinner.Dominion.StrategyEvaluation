module CardOrder (arbitraryCardOrder) where

import Card

-- There is no natural ordering of cards. However, it is necessary to be able to order them for the
-- purposes of tests which verify the total set, regardless of order. The actual order is not
-- important, as long as it is consistent.
arbitraryCardOrder :: Card -> String
arbitraryCardOrder = show
