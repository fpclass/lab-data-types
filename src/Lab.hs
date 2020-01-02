--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Data types                                                            --
--------------------------------------------------------------------------------

module Lab where 

--------------------------------------------------------------------------------

data IntPos = IntPos Int Int 

instance Eq IntPos where 
    (IntPos x0 y0) == (IntPos x1 y1) = undefined 

instance Show IntPos where 
    show (IntPos x y) = undefined 

zeroPos :: IntPos 
zeroPos = undefined 

x :: IntPos -> Int 
x = undefined 

y :: IntPos -> Int 
y = undefined 

--------------------------------------------------------------------------------

data Pos a = Pos a a 

instance Eq a => Eq (Pos a) where 
    (Pos x0 y0) == (Pos x1 y1) = undefined 

instance Show a => Show (Pos a) where 
    show (Pos x y) = undefined

zero :: Num a => Pos a 
zero = undefined 

left :: Pos a -> a 
left = undefined 

top :: Pos a -> a 
top = undefined 

--------------------------------------------------------------------------------

data DocumentItem = ListItem (Int -> String)

doc :: [DocumentItem]
doc = 
    [ ListItem (\n -> show n ++ ". An item")
    , ListItem (\n -> concat ["I am item #", show n])
    , ListItem (\n -> concat ["There. Are. ", show n, ". Items." ])
    ]

render :: [DocumentItem] -> Int -> String 
render = undefined

--------------------------------------------------------------------------------
