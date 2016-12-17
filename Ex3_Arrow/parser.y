{
module Parser where
import Scanner
}

%name calc
%tokentype { Scanner.Token }
%error { parseError }

%token
      "->"         { TArrow }
      '.'          { TDot }
      ','          { TComma }
      go           { TGo }
      take         { TTake }
      mark         { TMark }
      nothing      { TNothing }
      turn         { TTurn }
      case         { TCase }
      of           { TOf }
      end          { TEnd }
      left         { TLeft }
      right        { TRight }
      front        { TFront }
      ';'          { TSemicolon }
      empty        { TEmpty }
      lambda       { TLambda }
      debris       { TDebris }
      asteroid     { TAsteroid }
      boundary     { TBoundary }
      '_'          { TUnderscore }
      ident        { TIdent $$ }

%%
Program : Rules 			  { $1 }

Rules : {- empty -}			  { [] }
	  | Rule                  { [$1] }
      | Rules Rule            { $2 : $1 }

Rule : ident "->" Cmds '.'       { ($1, $3) }

Cmds : {- empty -}            { [] }
     | Cmd                    { [$1] }
     | Cmds ',' Cmd           { $3 : $1 }

Cmd : go                      { PGo }
    | take                    { PTake }
    | mark                    { PMark }
    | nothing                 { PNothing }
    | turn Dir                { PTurn $2 }
    | case Dir of Alts end    { PCase $2 $4 }
    | ident                   { PIdent $1 }

Dir : left                    { PLeft }
    | right                   { PRight }
    | front                   { PFront }

Alts : {- empty -}            { [] }
     | Alt                    { [$1] }
     | Alts ';' Alt           { $3 : $1 }

Alt : Pat "->" Cmds           { ($1, $3) }

Pat : empty                   { PEmpty }
    | lambda                  { PLambda }
    | debris                  { PDebris }
    | asteroid                { PAsteroid }
    | boundary                { PBoundary }
    | '_'                     { PUnderscore }

{
type PIdent = String

type Program = [Rule]
type Rule = (PIdent, Cmds)
type Cmds = [Cmd]
data Cmd = PGo | PTake | PMark | PNothing | PTurn Dir | PCase Dir Alts | PIdent String deriving (Show)
data Dir = PLeft | PRight | PFront deriving (Show)
type Alts = [Alt]
type Alt = (Pat, Cmds)
data Pat = PEmpty | PLambda | PDebris | PAsteroid | PBoundary | PUnderscore deriving (Show)

parseError :: [Token] -> a
parseError _ = error "parser error"
}