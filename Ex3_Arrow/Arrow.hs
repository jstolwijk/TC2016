--Jesse Stolwijk 4214676
module Arrow where

import Prelude hiding ((<*), (<$))

import ParseLib.Abstract
import Scanner
import Parser

import Data.List (find, sort)
import Data.Map (Map)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.Maybe (fromJust, fromMaybe)

type Space     =  Map Pos Contents
type Size      =  Int
type Pos       =  (Int, Int)
--data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary

parseSpace :: Parser Char Space
parseSpace =
  do
    (mr,mc)  <-  parenthesised
                   ((,) <$> natural <* symbol ',' <*> natural) <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <-  replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
             zipWith (\ r cs  ->
             zipWith (\ c d   ->  ((r,c),d)) [0..] cs) [0..] css

spaces :: Parser Char String
spaces = greedy (satisfy isSpace)

contents :: Parser Char Contents
contents =
  choice (Prelude.map (\ (f,c) -> f <$ symbol c) contentsTable) <* spaces

contentsTable :: [(Contents,Char)]
contentsTable =
  [  (Empty,'.'),(Lambda,'\\'),(Debris,'%'),(Asteroid,'O'),(Boundary,'#')]

-- These three should be defined by you
--type Ident = ()
type Commands = [Cmd]
data Heading = Up | Down | Left | Right deriving (Eq, Show)

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack
                 deriving (Show)

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String
           deriving (Show)
--test / debug function:
tarrow = "start  -> case left of Asteroid -> goOn; Boundary -> goOn; Lambda   -> turn left, go, take; _        -> turn left, go, start end. goOn   ->  case front of Asteroid -> turn right, goOn; Boundary -> turn right, goOn; Lambda   -> go, take; _        -> go, start end."
testFromFile p f = do
                        file <- readFile p
                        putStrLn (f file)

testParse = show . calc . alexScanTokens
testPrint = (\x ->  printSpace $ fst $ head $ parse parseSpace x)
testParseS s = calc (alexScanTokens s)

--ex 4
{-
"Happy User Guide" page 7 states:
"The only reason we used left recursion is that Happy is more efficient at parsing left-recursive rules; they result in a constant
stack-space parser, whereas right-recursive rules require stack space proportional to the length of the list being parsed. This can
be extremely important where long sequences are involved, for instance in automatically generated output. For example, the
parser in GHC used to use right-recursion to parse lists, and as a result it failed to parse some Happy-generated modules due to
running out of stack space!"

een gevolg hiervan is:
"One implication of using left recursion is that the resulting list comes out reversed, and you have to reverse it again to get it in
the original order. "

Bij parser combinators krijg je een oneindige loop bij links recursie
-}

--ex 5
--testAlgebra = foldProgramAlgebra (const) (testParseS tarrow)
type ProgramAlgebra p r cmd d alt c = ( 
                        [r] -> p,                                           --program
                        String -> [cmd] -> r,                               --rule
                        (Cmd -> cmd, d -> cmd, d -> [alt] -> cmd, String -> cmd),  --cmd algebra
                        d,                                                  --dir
                        c -> [cmd] -> alt,                                  --alt
                        Contents -> c                                       --contents
                     )

foldProgramAlgebra :: ProgramAlgebra p r cmd d alt c -> Program -> p
foldProgramAlgebra(p,r,(cmdNo,tu,ca,i), di, alt, con) = foldProgram 
      where foldProgram (Program rules) = p (map foldRule rules)
            foldRule (Rule ident cmds) = r ident (map foldCmd cmds)
            foldCmd (Turn dir) = tu (foldDir dir)
            foldCmd (Case dir alts) = ca (foldDir dir) (map foldAlt alts)
            foldCmd (Ident ident) = i ident
            foldCmd cmdX = cmdNo cmdX
            foldDir _ = di
            foldAlt (Alt contents cmds) = alt (con contents) (map foldCmd cmds)

--ex 6
check :: Program -> Bool
check p = all (==True) [checkCalls p, checkStart p, checkNoDouble p, checkExpressions p]

checkCalls :: Program -> Bool
checkCalls p = definedRules == calledRules
      where definedRules = sort $ foldProgramAlgebra definedRulesAlgebra p
            calledRules = sort $ removeDub $ foldProgramAlgebra cmdRulesAlgebra p

checkStart :: Program -> Bool
checkStart p = "start" `elem` foldProgramAlgebra definedRulesAlgebra p

checkNoDouble :: Program -> Bool
checkNoDouble p = not $ checkDub (foldProgramAlgebra definedRulesAlgebra p)

--Check expression not inplemented
checkExpressions :: Program -> Bool
checkExpressions p = True


--Helper functions
checkDub :: [String] -> Bool
checkDub xs = not $ length xs == length (removeDub xs)

removeDub :: [String] -> [String]
removeDub xs = foldr (\y ys -> if y `elem` ys then ys else (y:ys)) [] xs

 --Algebra for: Program -> [Rules Ident String]     (Get list of rule identifiers)                 
definedRulesAlgebra :: ProgramAlgebra [String] String () () () ()
definedRulesAlgebra = (id,
                        \s _ -> s,
                        (\_ -> (), \_ -> (), \_ _-> (), \_ -> ()),
                        (),
                        \_ _ -> (),
                        \_ -> ()
                       )

--Algebra for: Program -> [Cmd Ident String]    (Get list of cmd identifiers)
cmdRulesAlgebra :: ProgramAlgebra [String] [String] [String] () [String] ()
cmdRulesAlgebra = (concat,
                   \_ xs -> concat xs,
                   (\_ -> [], \_ -> [], \_ xs -> concat xs, (:[])),
                   (),
                   \_ xs -> concat xs,
                   \_ -> ()
                  )

{-
ttt = foldProgramAlgebra testAlgebra $ testParseS tarrow

testAlgebra :: ProgramAlgebra Bool Bool [Bool] () [(Contents, [[Contents]])] Contents
testAlgebra = (id,
                        \_ xs -> any (==False) xs,
                        (\_ -> [],\_ ->[], \_ xs -> tests xs, \_ -> []),
                        (),
                        \x xs -> (x, concat xs),
                        id
                        )
tests xs =  bevatElk(map fst xs) : concat (map snd xs)
bevatElk xs = all (\y -> y `elem` contentsList) xs
      where contentsList = [Empty, Lambda, Debris, Asteroid, Boundary, Underscore]
-}
--ex 7
printSpace :: Space -> String
printSpace s = printSize ++ L.foldlWithKey (\xs (_,k) v -> xs ++ [showContents v] ++ (checkNewLine k)) "" s
             where maxKey = maximum (L.keys s)
                   showContents x = fromJust (lookup x contentsTable) 
                   printSize = show maxKey ++ "\n"
                   checkNewLine k = if k == maxLength then "\n" else ""
                   maxLength = snd maxKey

--ex 8
toEnvironment :: String -> Environment
toEnvironment s = if check program then insertInto else raiseError
      where program@(Program rs) = calc (alexScanTokens s)
            insertInto = foldr (\(Rule i c) xs -> L.insert i c xs) L.empty rs
            raiseError = error "Can't create Environment"

--ex 9
step :: Environment -> ArrowState -> Step
step e state@(ArrowState sp pos h (st:sts)) = executeStep st
                                    where executeStep Go   = arrowStateToStep $ goStep state
                                          executeStep Take = arrowStateToStep $ takeStep state
                                          executeStep Mark = arrowStateToStep $ markStep state
                                          executeStep Parser.Nothing = arrowStateToStep $ (ArrowState sp pos h sts)
                                          executeStep (Turn d) = arrowStateToStep $ turnStep state d
                                          executeStep (Case d c) = caseStep state d c
                                          executeStep (Ident x) = ruleStep state e x

arrowStateToStep :: ArrowState -> Step
arrowStateToStep (ArrowState space pos dir []) = Done space pos dir
arrowStateToStep arrowstate                    = Ok arrowstate

goStep :: ArrowState -> ArrowState        
goStep (ArrowState space loc dir (_:sts)) = if checkPosition loc space 
                                                then ArrowState space (newPosition loc dir) dir sts  
                                                else ArrowState space loc dir sts             
takeStep :: ArrowState -> ArrowState
takeStep (ArrowState s p d (_:sts)) = if checkPosition p s 
                                                then ArrowState (L.insert p Empty s) p d sts
                                                else ArrowState s p d sts

markStep :: ArrowState -> ArrowState
markStep (ArrowState s p d (_:sts)) = ArrowState (L.adjust (const Lambda) p s) p d sts


turnStep :: ArrowState -> Dir -> ArrowState
turnStep (ArrowState s p d (_:sts)) dir = ArrowState s p (getHeading d dir) sts

caseStep :: ArrowState -> Dir -> [Alt] -> Step
caseStep (ArrowState s p d (_:sts)) dir as = case search getVPos of
                                                Just (Alt _ xs) -> arrowStateToStep (ArrowState s p d (xs ++ sts))
                                                Prelude.Nothing -> case search Underscore of
                                                             Just (Alt _ xs) -> arrowStateToStep (ArrowState s p d (xs ++ sts))
                                                             Prelude.Nothing -> Fail "No alternatives"
      where newPos = newPosition p (getHeading d dir)
            getVPos = s L.! newPos
            search x = find(\(Alt c _) -> c == x) as

ruleStep :: ArrowState -> Environment -> String -> Step
ruleStep (ArrowState s p d (_:sts)) e x = case L.lookup x e of
                                                Just r -> Ok (ArrowState s p d (r ++ sts))
                                                Prelude.Nothing -> Fail "Rule not found"
--helper functions
checkPosition :: (Int,Int) -> Space -> Bool
checkPosition p s = check $ fromMaybe Boundary find
      where find = L.lookup p s
            check Boundary = False
            check Asteroid = False
            check _ = True 
            
--moves location + heading = new location
newPosition :: (Int, Int) -> Heading -> (Int, Int)
newPosition (x,y) dir = fromJust (lookup dir [(Arrow.Up,(x,y-1)), (Arrow.Down, (x,y+1)), (Arrow.Left, (x-1,y)),(Arrow.Right, (x+1,y))])

--move 90degrees
getHeading :: Heading -> Dir -> Heading
getHeading x           Parser.Front = x
getHeading Arrow.Up    Parser.Left  = Arrow.Left
getHeading Arrow.Up    Parser.Right = Arrow.Right
getHeading Arrow.Down  Parser.Left  = Arrow.Right
getHeading Arrow.Down  Parser.Right = Arrow.Left
getHeading Arrow.Left  Parser.Left  = Arrow.Down
getHeading Arrow.Left  Parser.Right = Arrow.Up
getHeading Arrow.Right Parser.Left  = Arrow.Up
getHeading Arrow.Right Parser.Right = Arrow.Down


--debug ex 9-10
env1 = "start -> go,take,mark, case front of Lambda -> Go; _ -> start end."
env2 = "start -> go,take,mark, case front of Empty -> nothing; _ -> start end."
env3 = tarrow

space1 = fst $ head $ parse parseSpace "(3,3)\n....\n....\n....\n...."
state1 = ArrowState space1 (2,2) Arrow.Right [Ident "start"]
state2 = ArrowState space1 (1,2) Arrow.Up [Ident "start"]

deb = goStep state1

{-ex 10 
Size of the stack increases every deeper step into the recusrion.
If the recusrions takes place in the middle of the command sequence, the stack has to store all the new data caused by the recursion.
If the recursion takes place at the end of the command sequence, all the data is already in the stack and doesn't need to extend any further.

                  -}