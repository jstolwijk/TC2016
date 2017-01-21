Ex1:    0.5 /0.5   
CSharpLex.hs:
        ConstBool
        ConstChar
        lexConstBool
        lexConstChar
        lexToken, greedychoice:
            added new bool and char choice

CSharpCode.hs:
    fExprCon:
        bool
        char

Ex2: OpPriorities  1 / 1 
CSharpGram:
    opPriority
    pExpr 
    
Ex3:    1.5 / 1.5
CSharpAlgebra.hs:
    ExprMethod added

CSharpGram.hs:
    ExprMethod to Expr datatype
    pExprMethod

CSharpCode.hs:
    fExprMethod

Ex4:    0.5 / 0.5
CSharpCode.hs:
    fExprMethod ("print") added

Ex5:    / 1.5

Ex6:    / 2

Ex7: Comments   1/1
CSharpLex:
    lexCommentSpace
    lexComment

Ex8: Combined operators /1
CSharpGram:
    opCombined

CSharpLex:
    operators "+=" etc added

Ex9: For statement /1.5
Ex10: Algebra pretty print C# /1.5
Ex12: Class var /2
Ex13: Code genrator error messages /2