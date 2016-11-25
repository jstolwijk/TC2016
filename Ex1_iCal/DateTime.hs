import ParseLib.Abstract
import Data.Char as Char


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
--pDate :: Parser Char Date
pDate = Date <$> pYear <*> pMonth <*> pDay 

pYear = Year <$> parseInt 4
pMonth = Month <$> parseInt 2
pDay = Day <$> parseInt 2

--pTime :: Parser Char Time
pTime = Time <$> pHour <*> pMinute <*> pSecond
--pHour :: Parser Char Hour
pHour = Hour <$> (parseInt 2)
pMinute = Minute <$> parseInt 2
pSecond = Second <$> parseInt 2

--pUTC :: Parser Char Bool
--pUTC = True

--Int parse
numberGen :: [Int] -> Int
numberGen x = foldl(\x y -> 10*x + y) 0 x

parseInt :: Int -> String -> Int
parseInt n x = numberGen $ map digitToInt (take n x)

--Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = undefined -- DateTime <$> pDate <*> pTime <*> pUTC 

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run = undefined


-- Exercise 3
printDateTime :: DateTime -> String
printDateTime dt = undefined

printDate :: Date -> String
printDate = undefined

printTime :: Time -> String
printTime = undefined

printUTC :: Bool -> String
printUTC = undefined

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s



-- Exercise 5
dated = (Date (Year 1990) (Month 12) (Day 12))

checkDateTime :: DateTime -> Bool
checkDateTime dt =  checkDate (date dt) && 
                    checkTime (time dt)

checkDate :: Date -> Bool
checkDate (Date (Year y) (Month m) (Day d)) =   y < 2100 && y > 1900 && 
                                                m < 13 && 
                                                d < 32 

checkTime :: Time -> Bool
checkTime (Time (Hour h) (Minute m) (Second s)) =   h < 24 && 
                                                    m < 60 && 
                                                    s < 60

-- Exercise 6

