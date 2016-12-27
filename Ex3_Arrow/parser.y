{
module Parser where
import Scanner
}

%name calc
%tokentype { Scanner.Token }
%error { parseError }

%token
      "->"         { Scanner.TArrow }
      '.'          { Scanner.TDot }
      ','          { Scanner.TComma }
      go           { Scanner.TGo }
      take         { Scanner.TTake }
      mark         { Scanner.TMark }
      nothing      { Scanner.TNothing }
      turn         { Scanner.TTurn }
      case         { Scanner.TCase }
      of           { Scanner.TOf }
      end          { Scanner.TEnd }
      left         { Scanner.TLeft }
      right        { Scanner.TRight }
      front        { Scanner.TFront }
      ';'          { Scanner.TSemicolon }
      empty        { Scanner.TEmpty }
      lambda       { Scanner.TLambda }
      debris       { Scanner.TDebris }
      asteroid     { Scanner.TAsteroid }
      boundary     { Scanner.TBoundary }
      '_'          { Scanner.TUnderscore }
      ident        { Scanner.TIdent $$ }

%%
Program : Rules 			  { Program $1 }

Rule : ident "->" Cmds '.'       { Rule $1 $3 }

Rules : {- empty -}			  { [] }
	  | Rule                  { [$1] }
      | Rules Rule            { $2 : $1 }

Cmds : {- empty -}			  { [] }
	 | Cmd                   { [$1] }
     | Cmds ',' Cmd          { $3 : $1 }

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

Alt : Contents "->" Cmds      { Alt $1 $3 }

Contents : empty              { Empty }
    | lambda                  { Lambda }
    | debris                  { Debris }
    | asteroid                { Asteroid }
    | boundary                { Boundary }
    | '_'                     { Underscore }

{
type Ident = String

data Program = Program [Rule]			deriving (Show, Eq)
data Rule = Rule Ident [Cmd]			deriving (Show, Eq)

data Cmd = 	Go | 
			Take | 
			Mark | 
			Nothing | 
			Turn Dir | 
			Case Dir [Alt] | 
			Ident String 
			deriving (Show, Eq)

data Dir = 	Left | 
			Right | 
			Front 
			deriving (Show, Eq)

data Alt = Alt Contents [Cmd]
			deriving (Show, Eq)

data Contents =   Empty | 
			Lambda | 
			Debris | 
			Asteroid | 
			Boundary | 
			Underscore 
           	deriving (Eq, Show)
 

parseError :: [Token] -> a
parseError _ = error "parser error"
}