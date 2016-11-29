--Jesse Stolwijk 4214676
import Prelude hiding (sequence, (<*>), (<$>), (<$), parse, (<*))
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
    deriving (Eq, Ord)

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)


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
--parse string to DateTime (Date) (Time) (Utc)
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* parseTimeSeperator <*> parseTime <*> parseUTC 

-- Exercise 2
--get first succesfull (just) element from results if fail return nothing
run :: Parser a b -> [a] -> Maybe b
run p x = listToMaybe $ map fst $ parse p x
 
-- Exercise 3
--print (date ++ "t" ++ time ++ utc)
printDateTime :: DateTime -> String
printDateTime (DateTime (Date (Year y) (Month m) (Day d)) (Time (Hour h) (Minute mi) (Second s)) utc) 
    = f [y,m,d] ++ "T" ++ f [h,mi,s] ++ printUTC utc
        where f = concatMap showData

printUTC :: Bool -> String
printUTC True = "Z"
printUTC False = ""

--add 0 if number is a single digit
showData :: (Show a) => a -> String
showData a = if null xs then "0" ++ [x] else o
    where o@(x:xs) = show a

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s



-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime dt =  checkDate (date dt) && 
                    checkTime (time dt)

--check all date rules
checkDate :: Date -> Bool
checkDate (Date yo@(Year y) mo@(Month m) (Day d)) = all (==True) rules
    where rules = [y>0, y<9999, m>0, m<13, d>0, d<=daysInMonth mo yo]

--returns days in month (needs year for leap year check)                                       
daysInMonth :: Month -> Year -> Int
daysInMonth (Month m) y = ds !! (m - 1)
    where ds = [31,febDays y,31,30,31,30,31,31,30,31,30,31]

febDays:: Year -> Int
febDays y 
    | leapYear y = 29
    | otherwise = 28

leapYear :: Year -> Bool
leapYear (Year y)
    | modBool y 400 = True
    | modBool y 100 = False
    | modBool y 4 = True
    | otherwise = False

modBool :: Int -> Int -> Bool
modBool x y = mod x y == 0

--check time rules
checkTime :: Time -> Bool
checkTime (Time (Hour h) (Minute m) (Second s)) =   h < 24 && 
                                                    m < 60 && 
                                                    s < 60

-- Exercise 6
--save optional values as maybe (set nothing if no data)

{-
data event = Event {
                    uid :: String,
                    dtstamp :: DateTime,
                    dstart :: DateTime,
                    dend   :: DateTime,
                    decription :: Maybe String,
                    summary :: Maybe String,
                    location :: Maybe String
                    }    deriving (Eq, Ord)


data Calendar = Calendar {  propid :: String,
                            version :: String,
                            events :: [Event]
                          }    deriving (Eq, Ord)
-}