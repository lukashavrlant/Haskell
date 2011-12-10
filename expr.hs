data Expr = Const Int
    | Var Char
    | Add Expr Expr
    | Sub Expr Expr
    | Multi Expr Expr
    | Div Expr Expr
    deriving(Eq)
    
instance Show Expr where
    show (Const a) = show a
    show (Var x) = show x
    show (Add a b) = showBinary "+" a b
    show (Sub a b) = showBinary "-" a b
    show (Multi a b) = showBinary "*" a b
    show (Div a b) = showBinary "/" a b
    
showBinary op a b = "(" ++ (show a) ++ " " ++ op ++ " " ++ (show b) ++ ")"

zero (Const 0) = True
zero _ = False

one (Const 1) = True
one _ = False

sqr2 expr = Multi expr expr

bothConst (Const _) (Const _) = True
bothConst _ _ = False

doop op (Const a) (Const b) = Const (op a b)

inverse (Const a) = Const (-a)
inverse a = (Sub (Const 0) a)

simplifyHelper (a,b) = (simplify a, simplify b)

simplify expr@(Const _) = expr
simplify expr@(Var _) = expr

simplify expr@(Add a b)
    | zero sa = sb
    | zero sb = sa
    | bothConst sa sb = doop (+) sa sb
    | otherwise = Add sa sb
    where (sa, sb) = simplifyHelper(a, b)
          
simplify expr@(Sub a b)
    | zero sa = inverse sb
    | zero sb = sa
    | bothConst sa sb = doop (-) sa sb
    | otherwise = Sub sa sb
    where (sa, sb) = simplifyHelper(a, b)
    
simplify expr@(Multi a b)
    | (zero sa) || (zero sb) = (Const 0)
    | one sa = sb
    | one sb = sa
    | bothConst sa sb = doop (*) sa sb
    | otherwise = Multi sa sb
    where (sa, sb) = simplifyHelper(a, b)
    
simplify expr@(Div a b)
    | one sb = sa
    | zero sa = (Const 0)
    | otherwise = Div sa sb
    where (sa, sb) = simplifyHelper(a, b)
    
simderive = derive . simplify    

derive (Const _) = Const 0
derive (Var _) = Const 1
derive (Add a b) = simplify $ Add (simderive a) (simderive b)
derive (Sub a b) = simplify $ Sub (simderive a) (simderive b)
derive (Multi a b) = simplify $ (Add (Multi (simderive a) b) (Multi a (simderive b)))
derive (Div a b) = simplify $ (Div (Sub (Multi (simderive a) b) (Multi a (simderive b))) (sqr2 b))


subs _ _ expr@(Const _) = expr
subs var value expr@(Var x)
    | var == x = value
    | otherwise = expr
    
subs var value (Add a b) = subsHelper var value Add a b 
subs var value (Sub a b) = subsHelper var value Sub a b
subs var value (Multi a b) = subsHelper var value Multi a b
subs var value (Div a b) = subsHelper var value Div a b

subsHelper var value typ a b = typ (subs var value a) (subs var value b)

compute var value expr = simplify . subs var value $ expr
















