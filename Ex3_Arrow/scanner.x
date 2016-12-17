{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]   -- alphabetic characters
$plusmin    = [\+\-]     -- +-

tokens :-
$white+ ;
"--".* ;
\->         {const TArrow }
\.          {const TDot }
\,          {const TComma }
go          {const TGo }
take        {const TTake }
mark        {const TMark }
nothing     {const TNothing }
turn        {const TTurn }
case        {const TCase }
of          {const TOf }
end         {const TEnd }
left        {const TLeft }
right       {const TRight }
front       {const TFront }
\;          {const TSemicolon }
Empty       {const TEmpty }
Lambda      {const TLambda }
Debris      {const TDebris }
Asteroid    {const TAsteroid }
Boundary    {const TBoundary }
\_          {const TUnderscore }
[$alpha $digit $plusmin]+ { \s -> TIdent s }

{
-- Each action has type :: String -> Token
-- The token type:
data Token = TArrow | TDot | TComma | TGo | TTake | TMark | TNothing | TTurn | TCase | TOf | TEnd
    | TLeft | TRight | TFront | TSemicolon
    | TEmpty | TLambda | TDebris | TAsteroid | TBoundary | TUnderscore
    | TIdent String
    deriving (Eq,Show)

main = do
    s <- getContents
    print (alexScanTokens s)
}
