module Expr where
import ExprT
------------------------------ Exercise 3 ------------------------------

class Expr a where
    lit :: Integer -> a
    add, mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul