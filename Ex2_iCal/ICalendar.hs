module ICalendar where
import Prelude hiding (sequence, (<*>), (<$>), (<$), parse, (<*))
--http://hackage.haskell.org/package/pretty-1.1.3.4/docs/Text-PrettyPrint-HughesPJ.html#v:render
import Text.PrettyPrint.HughesPJ
import ParseLib.Abstract
import Data.Maybe


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


data Calendar = Calendar { prodId :: String
                         , events :: [VEvent] }
    deriving Eq

data VEvent = VEvent { dtStamp     :: DateTime
                     , uid         :: String
                     , dtStart     :: DateTime
                     , dtEnd       :: DateTime
                     , description :: Maybe String
                     , summary     :: Maybe String
                     , location    :: Maybe String }
    deriving Eq


run :: Parser a b -> [a] -> Maybe b
run p s = listToMaybe [p | (p, []) <- parse p s]

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar


-- "Main" block, DO NOT EDIT.
-- If you want to run the parser + pretty-printing, rename this module (first line) to "Main".
-- DO NOT forget to rename the module back to "ICalendar" before submitting to DomJudge.
main = do
    res <- readCalendar "examples/rooster_infotc.ics"
    putStrLn . render $ maybe (text "Calendar parsing error") (ppMonth (Year 2012) (Month 11)) res


-- Exercise 1
data Token = Token
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = undefined

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

--parse utc
parseUTC :: Parser Char Bool
parseUTC = True <$ symbol 'Z' <|> False <$ epsilon

numberGen :: [Int] -> Int
numberGen = foldl(\x y -> 10*x + y) 0

parseInt :: Int -> Parser Char Int
parseInt n = numberGen <$> sequence (replicate n newdigit)

parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* parseTimeSeperator <*> parseTime <*> parseUTC 

--stampParse :: Parser Char DateTime
stampParse x = sequence $ map symbol x
parseUid :: Parser Char String
parseUid = undefined

startCal :: Parser Char String
startCal = sequence (map symbol "ada" )

parseCalendar :: Parser Token Calendar
parseCalendar = undefined--Calendar <$> startCal parseDateTime <*> parseUid <*> parseDateTime <*> parseDateTime <*> parseUid <*> parseUid <*> parseUid



-- Exercise 2
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar = undefined


-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined


-- Exercise 4
countEvents :: Calendar -> Int
countEvents = undefined

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined



-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> Doc
ppMonth = undefined

