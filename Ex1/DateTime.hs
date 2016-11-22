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
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord,  Show)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord, Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord, Show)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord,Show)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord,Show)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord,Show)


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



-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = undefined

dateTime = DateTime (Date (parseDate)) (Time (parseTime)) (Bool False)
--parseTime :: Parser Char Time
parseTime = Time (Hour 10) (Minute 10) (Second 10)

--parseDate :: Parser Char Date
parseDate = Date (Year 1990) (Month 12) (Day 10)

numberGen :: [Int] -> Int
numberGen x = foldl(\x y -> 10*x + y) 0 x

stringParse :: String -> Int
stringParse x = numberGen $ map digitToInt x

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run = undefined


-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined



-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s




-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined



-- Exercise 6

