module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import Data.Map as M
import Data.Char
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM


data ValueOrAddress = Value | Address
    deriving Show

codeAlgebra :: CSharpAlgebra Code Code Code (ValueOrAddress -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprMethod)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

fMembMeth :: Type -> Token -> [Decl] -> Code -> Code
fMembMeth t (LowerId x) ps s = [LABEL x] ++ s ++ [RET]

fStatDecl :: Decl -> Code
fStatDecl d = []

fStatExpr :: (ValueOrAddress -> Code) -> Code
fStatExpr e = e Value ++ [pop]

fStatIf :: (ValueOrAddress -> Code) -> Code -> Code -> Code
fStatIf e s1 s2 = c ++ [BRF (n1 + 2)] ++ s1 ++ [BRA n2] ++ s2
    where
        c        = e Value
        (n1, n2) = (codeSize s1, codeSize s2)

fStatWhile :: (ValueOrAddress -> Code) -> Code -> Code
fStatWhile e s1 = [BRA n] ++ s1 ++ c ++ [BRT (-(n + k + 2))]
    where
        c = e Value
        (n, k) = (codeSize s1, codeSize c)

fStatReturn :: (ValueOrAddress -> Code) -> Code
fStatReturn e = e Value ++ [pop] ++ [RET]

fStatBlock :: [Code] -> Code
fStatBlock = concat

fExprCon :: Token -> ValueOrAddress -> Code
fExprCon (ConstInt n) va = [LDC n]
fExprCon (ConstBool True) va = [LDC (-1)]
fExprCon (ConstBool False) va = [LDC 0]
fExprCon (ConstChar x) va = [LDC (ord x)]
 
fExprVar :: Token -> ValueOrAddress -> Code
fExprVar (LowerId x) va = let loc = 37 in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]

fExprOp :: Token -> (ValueOrAddress -> Code) -> (ValueOrAddress -> Code) -> ValueOrAddress -> Code
fExprOp (Operator "=") e1 e2 va = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0]
fExprOp (Operator op)  e1 e2 va = e1 Value ++ e2 Value ++ [opCodes ! op]

fExprMethod :: Token -> (ValueOrAddress -> Code) -> ValueOrAddress -> Code
fExprMethod (Operator "print") = undefined--fExprOp ++ [TRAP 0]
fExprMethod o = undefined
    where l xs = Prelude.map (\x -> LDC x) xs
opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

