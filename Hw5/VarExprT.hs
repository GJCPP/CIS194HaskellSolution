{-# LANGUAGE FlexibleInstances #-}
module VarExprT where
import Expr
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Bits as HasVars
------------------------------ Exercise 6 ------------------------------

class HasVars a where
    var :: String -> a

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    -- "lit x" is interpreted as a constant map, which ignores the input and return x
    lit x _ = Just x
    add x y map = if isJust vx && isJust vy then Just $ fromJust vx + fromJust vy else Nothing
        where vx = x map
              vy = y map
    mul x y map = if isJust vx && isJust vy then Just $ fromJust vx * fromJust vy else Nothing
        where vx = x map
              vy = y map


withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

{-
    >> :t add (lit 3) (var "x")
    add (lit 3) (var "x") :: (Expr a, HasVars a) => a

    Explanation: the entire expression is only an instance of Expr and HasVars.
        (Which instance is not specified.)
        This means, we can take it as ANY type being instance of Expr and HasVars.

        Now another view is that an expression with variable is a map from a String-Integer table to (Maybe) Integer.
        E.g. literal c is a constant map f that always maps to c; "add (var "x") (lit 1)" is a map f(x) = x + 1.

    Personal remark:
        If you don't write it, you don't see how astonishing it is...
-}