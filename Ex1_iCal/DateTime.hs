import Prelude hiding (sequence, (<*>), (<$>), (<$), parse)
import ParseLib.Abstract
import Data.Char as Char
import Data.Maybe
-- Starting Framework


-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord, Show)

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord, Show)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord, Show)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord,Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord, Show)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord, Show)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord, Show)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord, Show)


-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show

main2 = printOutput . processCheck . processInput
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show

-- Exercise 1 helper functions

--parse date
parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay 

parseYear :: Parser Char Year
parseYear = Year <$> parseInt 4

parseMonth :: Parser Char Month
parseMonth = Month <$> parseInt 2

parseDay :: Parser Char Day
parseDay = Day <$> parseInt 2


--Parse time
parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseHour :: Parser Char Hour
parseHour = Hour <$> parseInt 2

parseMinute :: Parser Char Minute
parseMinute = Minute <$> parseInt 2

parseSecond :: Parser Char Second
parseSecond = Second <$> parseInt 2

parseTimeSeperator :: Parser Char Char  
parseTimeSeperator = symbol 'T'

parseUTC :: Parser Char Bool
parseUTC = True <$ symbol 'Z' <|> False <$ epsilon

--Int parse
--cast list of int to number [1,2,3] -> 123
--functie uit de refresh ex0
numberGen :: [Int] -> Int
numberGen = foldl(\x y -> 10*x + y) 0


--stringParse :: String -> Int
--stringParse x = numberGen $ map digitToInt x


parseInt :: Int -> Parser Char Int
parseInt n = numberGen <$> sequence (replicate n newdigit)

--Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* parseTimeSeperator <*> parseTime <*> parseUTC 

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run parser x = listToMaybe (map fst (parse parser x))
 

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime d t u) = printDate d ++ printTime t ++ printUTC u

printDate :: Date -> String
printDate d = show' (unYear (year d)) ++ show' (unMonth (month d)) ++ show' (unDay (day d))

printTime :: Time -> String
printTime d = "T" ++ show' (unHour (hour d)) ++ show' (unMinute (minute d)) ++ show' (unSecond (second d))

printUTC :: Bool -> String
printUTC True = "Z"
printUTC False = ""

show' :: (Show a) => a -> String
show' x = if ss == [] then "0" ++ [s] else sss
    where sss@(s:ss) = show x
-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s



-- Exercise 5
dated = DateTime (Date (Year 1990) (Month 12) (Day 12)) (Time (Hour 10) (Minute 10) (Second 10)) True

checkDateTime :: DateTime -> Bool
checkDateTime dt =  checkDate (date dt) && 
                    checkTime (time dt)

checkDate :: Date -> Bool
checkDate (Date (Year y) (Month m) (Day d)) = if m == 2 && d == 29
                                              then leapYear y
                                              else
                                                y > 0       && 
                                                y < 9999    &&
                                                m > 0       &&
                                                m < 13      && 
                                                d > 0        &&
                                                d < 32 

                                                
leapYear :: Int -> Bool
leapYear a = (check a 4) && (not (check a 100)) && (check a 400)

check :: Int -> Int -> Bool
check x y = (x `div` y) == 0

checkTime :: Time -> Bool
checkTime (Time (Hour h) (Minute m) (Second s)) =   h < 24 && 
                                                    m < 60 && 
                                                    s < 60

-- Exercise 6

