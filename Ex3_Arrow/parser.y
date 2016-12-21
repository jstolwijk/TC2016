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

Cmd : go                      { Go }
    | take                    { Take }
    | mark                    { Mark }
    | nothing                 { Parser.Nothing }
    | turn Dir                { Turn $2 }
    | case Dir of Alts end    { Case $2 $4 }
    | ident                   { Ident $1 }

Dir : left                    { Parser.Left }
    | right                   { Parser.Right }
    | front                   { Front }

Alts : {- empty -}            { [] }
     | Alt                    { [$1] }
     | Alts ';' Alt           { $3 : $1 }

Alt : Contents "->" Cmds           { ($1, $3) }

Contents : empty                   { Empty }
    | lambda                  { Lambda }
    | debris                  { Debris }
    | asteroid                { Asteroid }
    | boundary                { Boundary }
    | '_'                     { Underscore }

{
type Ident = String

type Program = [Rule]
type Rule = (Ident, Cmds)
type Cmds = [Cmd]

data Cmd = 	      Go | 
			Take | 
			Mark | 
			Nothing | 
			Turn Dir | 
			Case Dir Alts | 
			Ident String 
			deriving (Show)

data Dir = 	      Left | 
			Right | 
			Front 
			deriving (Show)

type Alts = [Alt]
type Alt = (Contents, Cmds)

data Contents =   Empty | 
			Lambda | 
			Debris | 
			Asteroid | 
			Boundary | 
			Underscore 
			deriving (Show)

parseError :: [Token] -> a
parseError _ = error "parser error"
}