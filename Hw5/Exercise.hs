{-# LANGUAGE TypeSynonymInstances #-}
module Exercise where


import ExprT
import Parser
import Expr
import VarExprT
import qualified StackVM
import GHC.Integer (Integer)
import Control.Category (Category(id))
import StackVM (stackVM)
import qualified Data.Bits as Expr
import qualified Data.Map as M

------------------------------ Exercise 2 ------------------------------

eval :: ExprT -> Integer
eval (ExprT.Lit val) = val
eval (ExprT.Add exp1 exp2) = eval exp1 + eval exp2
eval (ExprT.Mul exp1 exp2) = eval exp1 * eval exp2

evalStr :: String -> Maybe Integer
evalStr s = maybe Nothing (Just . eval) (parseExp ExprT.Lit ExprT.Add ExprT.Mul s)

------------------------------ Exercise 4 ------------------------------

-- Oh God so it is a template...
instance Expr Integer where
    lit = Prelude.id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

------------------------------ Exercise 5 ------------------------------

instance Expr StackVM.Program where
    lit val = [StackVM.PushI val]
    add pro1 pro2 = pro1 ++ pro2 ++ [StackVM.Add]
    mul pro1 pro2 = pro1 ++ pro2 ++ [StackVM.Mul]

-- So we can convert ExprT to any type in class Expr...
evalExprT :: Expr a => ExprT -> a
evalExprT (ExprT.Lit val) = lit val
evalExprT (ExprT.Add exp1 exp2) = add (evalExprT exp1) (evalExprT exp2)
evalExprT (ExprT.Mul exp1 exp2) = mul (evalExprT exp1) (evalExprT exp2)

compile :: String -> Maybe StackVM.Program
compile s = maybe Nothing (Just . evalExprT) (parseExp ExprT.Lit ExprT.Add ExprT.Mul s)


