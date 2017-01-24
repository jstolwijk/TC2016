module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import Data.Map as M
import Data.Char
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import Text.PrettyPrint


data ValueOrAddress = Value | Address
    deriving Show
    
type Environment = Map String Int
--local var are defined by (Environment -> Code)
--parms var are defined by ParamEnv
type ParamEnv = (ValueOrAddress -> Environment -> Code)
 
codeAlgebra :: CSharpAlgebra Code Code (Environment -> Code) ParamEnv
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatFor, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprOpSingle, fExprMethod)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

fMembMeth :: Type -> Token -> [Decl] -> (Environment -> Code) -> Code
fMembMeth t (LowerId x) ps s = [LABEL x, LINK 0] ++ s env ++ [UNLINK, RET]
    where  env = fromList $ zip [x | (Decl _ (LowerId x)) <- ps] [(-(length ps) - 1)..]

fStatDecl :: Decl -> (Environment -> Code)
fStatDecl d env = []

fStatExpr ::  ParamEnv -> (Environment -> Code)
fStatExpr e env = e Value env ++ [pop]

fStatIf :: ParamEnv -> (Environment -> Code) -> (Environment -> Code) -> (Environment -> Code)
fStatIf e s1 s2 env = c ++ [BRF (n1 + 2)] ++ (s1 env) ++ [BRA n2] ++ (s2 env)
    where
         c        = e Value env
         (n1, n2) = (codeSize (s1 env), codeSize (s2 env))

fStatWhile :: ParamEnv -> (Environment -> Code) -> (Environment -> Code)
fStatWhile e s1 env = [BRA n] ++ (s1 env) ++ c ++ [BRT (-(n + k + 2))]
    where
            c = e Value env
            (n, k) = (codeSize (s1 env), codeSize c)

fStatFor = undefined

fStatReturn :: ParamEnv -> (Environment -> Code)
fStatReturn e env = e Value env ++ [STR R3, UNLINK, RET]

fStatBlock :: [(Environment -> Code)] -> (Environment -> Code)
fStatBlock va env = concatMap ($ env) va

--ConstChar and ConstBool added
fExprCon :: Token -> ParamEnv
fExprCon (ConstInt n) _ _       = [LDC n]
fExprCon (ConstBool True) _ _   = [LDC 1]
fExprCon (ConstBool False) _ _  = [LDC 0]
fExprCon (ConstChar x) _ _      = [LDC (ord x)]
 
--added check if value is in env
fExprVar :: Token -> ParamEnv
fExprVar (LowerId x) va env = [(t va) loc]
    where   t Value = LDL
            t Address = LDLA
            loc = if member x env then env ! x else 37

fExprOp :: Token -> ParamEnv -> ParamEnv -> ParamEnv
fExprOp (Operator "=") e1 e2 va env = e2 Value env ++ [LDS 0] ++ e1 Address env ++ [STA 0]
fExprOp (Operator op)  e1 e2 va env = e1 Value env ++ e2 Value env ++ [opCodes ! op]

fExprOpSingle :: Token -> ParamEnv -> ParamEnv
fExprOpSingle (Operator x) e1 va env = e1 Value env ++ [LDC 1, f x] ++ [LDS 0] ++ e1 Address env ++ [STA 0]
    where f "++" = ADD
          f "--" = SUB

fExprMethod :: Token -> [ParamEnv] -> ParamEnv
fExprMethod (LowerId "print") e1 va env = concatMap (\x -> x Value env) e1 ++ replicate (length e1) (TRAP 0)  ++ [LDR R3]
fExprMethod (LowerId n) e1 va env = concatMap (\x -> x Value env) e1 ++ [Bsr n] ++ [AJS (-(length e1)), LDR R3]

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

{-
printAlgebra :: CSharpAlgebra (Token -> [String] -> String) String String String
printAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprMethod)
    )
    where
            fClas c ms = "a"
            fMembDecl d = ""
            fMembMeth t (LowerId x) ps s = ""
            fStatDecl d env = ""
            fStatExpr e env = ""
            fStatIf e s1 s2 env = ""
            fStatWhile e s1 env = ""
            fStatReturn e env = ""
            fStatBlock v env = ""
            fExprCon (ConstInt n) _ _ = ""
            fExprVar (LowerId x) va env = ""
            fExprOp (Operator op)  e1 e2 _ env = ""
            fExprMethod (LowerId "print") ps _ env = ""
-}

