data Val = I Int | C Char | P (Int,Char) deriving Show
data MList = M [Val]

filter' :: Char -> MList -> MList
filter' c (M l) = M $ filter (f c) l
    where 
        f 'i' (I _) = True
        f 'c' (C _) = True
        f 'p' (P _) = True
        f _ _ = False

conv :: MList -> Maybe [Char]
conv (M ((C c):rest)) =
    case (conv (M rest)) of
        Just l -> Just (c:l)
        Nothing -> Nothing
conv (M []) = Just []
conv _ = Nothing