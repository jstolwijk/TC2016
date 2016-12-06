module ICalendar where
import Prelude hiding (sequence, (<*>), (<$>), (<$), parse, (<*), (*>))
import Text.PrettyPrint
import ParseLib.Abstract
import Data.Maybe
import Data.Char

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
data Token = Tstartcal 
            | Tendcal
            | Tstartevent
            | Tendevent
            | Tdtstamp DateTime
            | Tdtstart DateTime
            | Tdtend DateTime
            | Tuid String
            | Tsummary String
    deriving (Eq, Ord)
spaces :: Parser Char String
spaces = greedy (satisfy isSpace)

tstartcal :: Parser Char Token
tstartcal = Tstartcal <$ token "BEGIN:VCALENDAR"

tendcal :: Parser Char Token
tendcal = Tendcal <$ token "END:VCALENDAR"

tstartevent :: Parser Char Token
tstartevent = Tstartevent <$ token "BEGIN:VEVENT"

tendevent :: Parser Char Token
tendevent = Tendevent <$ token "END:VEVENT"

tstuid :: Parser Char Token
tstuid = undefined --Tuid <$ token "UID:" -- <*> token

tdtstamp :: Parser Char Token
tdtstamp = Tdtstamp <$ token "DTSTAMP:" <*> parseDateTime

tdtstart :: Parser Char Token
tdtstart = Tdtstart <$ token "DTSTART:" <*> parseDateTime

tdtend :: Parser Char Token
tdtend = Tdtend <$ token "DTEND:" <*> parseDateTime

anyToken :: Parser Char Token
anyToken = tstartcal <|> tstartevent <|> tdtstamp <|> tdtstart <|> tdtend <|> tendevent <|> tendcal

scanCalendar :: Parser Char [Token]
scanCalendar = spaces *> greedy anyToken <* eof

--parse date
parseDate :: Parser Char Date
parseDate = Date <$> (Year <$> parseInt 4) <*> (Month <$> parseInt 2) <*> (Day <$> parseInt 2) 


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

parseUid :: Parser Char String
parseUid = undefined

{-
datetime :: Token -> DateTime
datetime t = fromDateTime <$> (satisfy isDateTime t)

isDateTime :: Token -> Bool
isDateTime (DateTime x) = True
isDateTime _= False

fromDateTime :: Token -> DateTime
fromDateTime (DateTime x) = x
fromDateTime _ = error "fromDateTime"-}
{-
checkString :: Token -> String
checkString = fromString <$> satisfy IsString

isString :: Token -> Bool
isString t :: [Char] = True
isString _ = False
fromString :: Token -> String
fromString t :: [Char] = t
fromString _ = error "fromDateTime"-}
parseCalendar :: Parser Token Calendar
parseCalendar = undefined--Calendar <$> datetime <*> checkString <*> datetime <*> datetime <*> checkString <*> checkString <*> checkString

parseVEvent :: Parser Token VEvent
parseVEvent = undefined--VEvent <$> 


-- Exercise 2
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar = undefined


dt = (DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 17) (Minute 01) (Second 01)) True) 
dt' = (DateTime (Date (Year 1997) (Month 07) (Day 17)) (Time (Hour 17) (Minute 01) (Second 02)) True) 
dt2 = (DateTime (Date (Year 1997) (Month 07) (Day 15)) (Time (Hour 17) (Minute 01) (Second 02)) True) 

vev = (VEvent dt "19970610T172345Z-AF23B2@example.com" dt dt' (Just "a") (Just "a") (Just "a" ))
exCal = Calendar "-//hacksw/handcal//NONSGML v1.0//EN" [vev]
-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar (Calendar pid xs) = printBeginCal++ "\r\n" ++ "PRODID:" ++ pid ++ "\r\n"++ concatMap printVEvent xs ++  printEndCal 

printVEvent :: VEvent -> String
printVEvent (VEvent stamp uid s e d summ loc) = printBeginEvent ++ "\r\n" ++
                                                "DTSTAMP:" ++ printDateTime stamp ++ "\r\n"++ 
                                                "DTSTART:" ++ printDateTime s++ "\r\n" ++ 
                                                "DTEND:" ++ printDateTime e ++ "\r\n" ++ 
                                                "UID:" ++ uid ++ "\r\n"++ 
                                                printMaybe d "DESCRIPTION:" ++ 
                                                printMaybe summ "SUMMARY:" ++ 
                                                printMaybe loc "LOCATION:" ++
                                                printEndEvent ++ "\r\n"

printBeginCal = "BEGIN:VCALENDAR"
printEndCal = "END:VCALENDAR"
printBeginEvent = "BEGIN:VEVENT"
printEndEvent = "END:VEVENT"

printMaybe (Just x) p = p ++ x ++ "\r\n"
printMaybe Nothing _ = ""

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
countEvents :: Calendar -> Int
countEvents (Calendar pid xs) = length xs

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents dt (Calendar _ xs) = foldr (checkEvent dt) [] xs

checkEvent :: DateTime -> VEvent -> [VEvent] -> [VEvent]
checkEvent dt e xs = if start < dt && end > dt then e:xs else xs
    where start = dtStart e
          end = dtEnd e


checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent s (Calendar _ xs) = foldr (\c x -> if fromJust'(summary c) == s then x + (minusDateTime (dtStart c) (dtEnd c)) else x) 0 xs
    where fromJust' (Just x) = x
          fromJust' _ = ""
minusDateTime (DateTime (Date (Year y) (Month m) (Day d)) (Time (Hour h) (Minute mi) (Second s)) _) 
              (DateTime (Date (Year y2) (Month m2) (Day d2)) (Time (Hour h2) (Minute mi2) (Second s2)) _) 
              = (d2 - d) * 60 * 24 + (h2 - h) * 60 + mi2 - mi + quot (s2 - s) 60



data EventWeek = EventWeek { eventdays :: [EventDay]}

data EventDay = EventDay {  unDate :: Date,
                            unEvents :: [VEvent] }

-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> Doc
ppMonth =undefined

createEventWeek :: Calendar -> Date -> EventWeek
createEventWeek c d = EventWeek $ map (findEventsDay c) (createDateList d dayList)
    where dayList = [1..(daysInMonth (month d) (year d))]

findEventsDay :: Calendar -> Date -> EventDay
findEventsDay (Calendar _ xs) d = EventDay (d) (foldr (\e as -> if date (dtStart e) == d then e : as else as) [] xs)

createDateList :: Date -> [Int] -> [Date]
createDateList (Date (Year y) (Month m) (Day d)) ds = foldr (\c xs -> ((Date (Year y) (Month m) (Day c)):xs)) [] ds


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
