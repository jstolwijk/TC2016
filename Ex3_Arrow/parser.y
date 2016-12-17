{
module Main where
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      ->          { TArrow }
      '.'         { TDot }
      ','         { TComma }
      go          { TGo }
      take        { TTake }
      mark        { TMark }
      nothing     { TNothing }
      turn        { TTurn }
      case        { TCase }
      of          { TOf }
      end         { TEnd }
      left        { TLeft }
      right       { TRight }
      front       { TFront }
      ';'         { TSemicolon }
      Empty       { TEmpty }
      Lambda      { TLambda }
      Debris      { TDebris }
      Asteroid    { TAsteroid }
      Boundary    { TBoundary }
      '_'         { TUnderscore }
      TIdent      { TIdent }

%% 

