import Data.Char
import System.IO
import System.Environment
import Data.List
import System.IO.Error

---------- Input/Output ----------
main = tryMain `catch` handler

tryMain = do
    args <- getArgs
    if length args > 0 then do
        contents <- readFile $ head args
        writeFile ("derive_" ++ head args) $ unlines $ map (show.derive.parse) $ lines contents
    else
        interact $ unlines . map (show . derive . parse) . lines 
        
handler e = putStrLn "File does not exits" 

---------- Data definiton ----------
data Expr a = Const a
    | Var Char
    | Add (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Multi (Expr a) (Expr a)
    | Div (Expr a) (Expr a)
    deriving(Eq)
    
instance (Show e) => Show (Expr e) where
    show (Const x) = show x
    show (Var x) = show x
    show (Add a b) = showBinary "+" a b
    show (Sub a b) = showBinary "-" a b
    show (Div a b) = showBinary "/" a b
    show (Multi a b) = showBinary "*" a b
    
    
showBinary op a b = "(" ++ (show a) ++ " " ++ op ++ " " ++ (show b) ++ ")"


---------- Functors ----------
instance Functor Expr where  
    fmap f (Const a) = Const (f a)
    fmap f (Var a) = Var a
    fmap f (Add a b) = Add (fmap f a) (fmap f b)
    fmap f (Sub a b) = Sub (fmap f a) (fmap f b)
    fmap f (Multi a b) = Multi (fmap f a) (fmap f b)
    fmap f (Div a b) = Div (fmap f a) (fmap f b)


---------- Help functions ----------
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

normalize (Const a) (Const b)
    | b == 0 = error "Division by Zero"
    | d == (Const 1) = n
    | otherwise = (Div n d)
    where   g = gcd a b
            n = (Const $ a `div` g)
            d = (Const $ b `div` g)


---------- Simplify expression ----------
simplifyHelper (a,b) = (simplify a, simplify b)

simplify expr@(Add a b)
    | zero sa = sb
    | zero sb = sa
    | sa == sb = (Multi (Const 2) sa)
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
    | bothConst sa sb = normalize sa sb
    | otherwise = Div sa sb
    where (sa, sb) = simplifyHelper(a, b)
    
simplify expr = expr

---------- Derive expression ----------
simderive = derive . simplify    

derive (Const _) = Const 0
derive (Var _) = Const 1
derive (Add a b) = simplify $ Add (simderive a) (simderive b)
derive (Sub a b) = simplify $ Sub (simderive a) (simderive b)
derive (Multi a b) = simplify $ (Add (Multi (simderive a) b) (Multi a (simderive b)))
derive (Div a b) = simplify $ (Div (Sub (Multi (simderive a) b) (Multi a (simderive b))) (sqr2 b))


---------- Parsing ----------
parse = getPureExpr . parseLex . (map Left) . words . removeSpaces . replace "+-*/()"


parseLex [expr] = Right $ str2Expr expr
parseLex lexems
    | b == 0 = head lexems
    | otherwise = parseLex (makeExpr lexems pair)
    where pair@(a,b) = findPair lexems

makeExpr lexems (a, b) = (takeFromTo 0 a lexems) ++ [newExpr] ++ (takeFromTo (b+1) (length lexems) lexems)
    where newExpr = (getExpr (takeFromTo (a+1) b lexems))
  
getExpr [left, op, right]
    | op == Left "+" = Right $ Add (str2Expr left) (str2Expr right)
    | op == Left "-" = Right $ Sub (str2Expr left) (str2Expr right)
    | op == Left "*" = Right $ Multi (str2Expr left) (str2Expr right)
    | op == Left "/" = Right $ Div (str2Expr left) (str2Expr right)
    
takeFromTo 0 to xs = take to xs
takeFromTo from to (x:xs) = takeFromTo (from-1) (to-1) xs

str2Expr (Right x) = x
str2Expr (Left expr@(x:xs)) 
    | isLetter x = Var x
    | otherwise = Const (read expr :: Int)

getPureExpr (Right a) = a

findPair lexems = findRight lexems (0,0) 0
findRight [] x  _ = x
findRight (x:xs) (a, b) i
    | x == Left "(" = findRight xs (i, b) (i+1)
    | x == Left ")" = (a, i)
    | otherwise = findRight xs (a, b) (i+1)
    
replace _ [] = []
replace chars (x:xs)
    | (find (==x) chars) == Nothing = x : (replace chars xs)
    | otherwise = ' ' : x : ' ' : (replace chars xs)
    
removeSpaces [] = []
removeSpaces (' ':' ':xs) = removeSpaces $ ' ':xs
removeSpaces (x:xs) = x : removeSpaces xs