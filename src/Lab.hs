--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Data types                                                            --
--------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Lab where 

--------------------------------------------------------------------------------

data IntPos = IntPos {
    x :: Int,
    y :: Int
} -- deriving (Eq, Show)

instance Eq IntPos where 
    (IntPos x0 y0) == (IntPos x1 y1) = x0 == x1 && y0 == y1 

-- show (IntPos 4 7) => "IntPos 4 7"
instance Show IntPos where 
    -- show (IntPos x y) = "IntPos " ++ show x ++ " " ++ show y
    show IntPos{..} = concat [ "IntPos ", show x, " ", show y ]

zeroPos :: IntPos 
zeroPos = IntPos 0 0 

-- x :: IntPos -> Int 
-- x (IntPos v _) = v 

-- y :: IntPos -> Int 
-- y (IntPos _ v) = v 

--------------------------------------------------------------------------------

data Pos a = Pos {
    left :: a,
    top :: a
}
    -- deriving (Eq, Show)

instance Eq a => Eq (Pos a) where 
    (Pos x0 y0) == (Pos x1 y1) = 
        x0 == x1 && y0 == y1 

instance Show a => Show (Pos a) where 
    show (Pos x y) = concat [ "Pos ", show x, " ", show y ]

zero :: Num a => Pos a 
zero = Pos 0 0 

-- left :: Pos a -> a 
-- left (Pos v _) = v 

-- top :: Pos a -> a 
-- top (Pos _ v) = v 

--------------------------------------------------------------------------------

data DocumentItem = ListItem (Int -> String)

doc :: [DocumentItem]
doc = 
    [ ListItem (\n -> show n ++ ". An item")
    , ListItem (\n -> concat ["I am item #", show n])
    , ListItem (\n -> concat ["There. Are. ", show n, ". Items." ])
    ]

render :: [DocumentItem] -> Int -> String 
render [] _ = ""
render (ListItem f : xs) n = f n ++ "\n" ++ render xs (n+1)
-- render xs n = foldr (\(i,ListItem f) r -> f i ++ "\n" ++ r) "" (zip [n..] xs)

--------------------------------------------------------------------------------
