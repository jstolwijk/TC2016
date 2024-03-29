{-
=== Exercise 1
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 1 tests.
+++ OK, passed 100 tests.
=== Exercise 4
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
*** Failed! Falsifiable (after 3 tests): 
Right BEGIN:VCALENDAR
PRODID:y
VERSION:2.0
BEGIN:VEVENT
DTSTAMP:12511018T092755Z
UID:L
DTSTART:20620101T021807Z
DTEND:33870212T225010Z
DESCRIPTION:D
END:VEVENT
BEGIN:VEVENT
DTSTAMP:15660820T084717Z
UID:uq
DTSTART:24150610T193826Z
DTEND:95730125T234834Z
DESCRIPTION:g
 ae
END:VEVENT
END:VCALENDAR

de bonus opdracht is multiline en bij opgave 4 wordt het gewoon fout gerekend terwijl multiline bonus is :(

Jesse Stolwijk 4214676
-}


module ICalendar where
import Prelude hiding (sequence, (<*>), (<$>), (<$), parse, (<*), (*>))
import Text.PrettyPrint
import ParseLib.Abstract
import Data.Maybe
import Data.Char
import System.IO
import Data.Text.Unsafe
import Data.List.Split
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


recognizeToken s = listToMaybe  [scanCalendar | (scanCalendar, []) <- parse scanCalendar s]
 
-- "Main" block, DO NOT EDIT.
-- If you want to run the parser + pretty-printing, rename this module (first line) to "Main".
-- DO NOT forget to rename the module back to "ICalendar" before submitting to DomJudge.
main = do
    res <- readCalendar "examples/bastille.ics"
    putStrLn . render $ maybe (text "Calendar parsing error") (ppMonth (Year 2012) (Month 11)) res

aaaa = fromJust $ recognizeCalendar "BEGIN:VCALENDAR\r\nPRODID:w\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nDTSTAMP:64351217T223104Z\r\nUID:z\r\nDTSTART:46320421T170257Z\r\nDTEND:66390220T153130Z\r\nEND:VEVENT\r\nEND:VCALENDAR"
-- Exercise 1
data Token =Tstartcal 
            | Tprodid String
            | Tendcal
            | Tstartevent
            | Tendevent
            | Tdtstamp DateTime
            | Tdtstart DateTime
            | Tdtend DateTime
            | Tuid String
            | Tlocation String
            | Tdescription String
            | Tsummary String
            | Tempty
    deriving (Eq, Ord)

--parse date
parseDate :: Parser Char Date
parseDate = Date <$> (Year <$> pInt 4) <*> (Month <$> pInt 2) <*> (Day <$> pInt 2) 

--Parse time
parseTime :: Parser Char Time
parseTime = Time <$> (Hour <$> pInt 2) <*> (Minute <$> pInt 2) <*> (Second <$> pInt 2)

--parse utc
parseUTC :: Parser Char Bool
parseUTC = True <$ symbol 'Z' <|> False <$ epsilon

parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* symbol 'T' <*> parseTime <*> parseUTC

--Deze variabelen worden bij het lexen en het printen gebruikt
printBeginCal = "BEGIN:VCALENDAR"
printVersion = "VERSION:2.0"
printProdID = "PRODID:"
printBeginEvent = "BEGIN:VEVENT"
printDtStamp = "DTSTAMP:"
printDtStart = "DTSTART:"
printDtEnd = "DTEND:"
printUID = "UID:"
printSummary = "SUMMARY:"
printLocation = "LOCATION:" 
printDescription = "DESCRIPTION:" 
printEndEvent = "END:VEVENT"
printEndCal = "END:VCALENDAR"
printCrlf = "\r\n" 


--scanner
anyToken :: Parser Char Token
anyToken = Tstartcal    <$ token printBeginCal                              <* newLine <|>
           Tempty       <$ token printVersion                               <* newLine <|>
           Tprodid      <$ token printProdID        <*> readStringTillEnd   <* newLine <|>
           Tstartevent  <$ token printBeginEvent                            <* newLine <|> 
           Tdtstamp     <$ token printDtStamp       <*> parseDateTime       <* newLine <|> 
           Tdtstart     <$ token printDtStart       <*> parseDateTime       <* newLine <|> 
           Tdtend       <$ token printDtEnd         <*> parseDateTime       <* newLine <|> 
           Tuid         <$ token printUID           <*> readStringTillEnd   <* newLine <|>
           Tsummary     <$ token printSummary       <*> readStringTillEnd   <* newLine <|>
           Tlocation    <$ token printLocation      <*> readStringTillEnd   <* newLine <|>
           Tdescription <$ token printDescription   <*> readStringTillEnd   <* newLine <|>
           Tendevent    <$ token printEndEvent                              <* newLine <|> 
           Tendcal      <$ token printEndCal                                           <|> 
           Tendcal      <$ token printEndCal                                <* newLine
           where newLine = token printCrlf

scanCalendar :: Parser Char [Token]
scanCalendar = greedy anyToken <* eof


numberGen :: [Int] -> Int
numberGen = foldl(\x y -> 10*x + y) 0

--parse n ints
pInt :: Int -> Parser Char Int
pInt n = numberGen <$> sequence (replicate n newdigit)

--read string till line end \r\n
readStringTillEnd :: Parser Char String
readStringTillEnd = many $ satisfy (\x -> x /= '\n' && x /= '\r')

{-
--vb van slides
station :: Parser Token Station
station = fromStation <$> satisfy isStation
isStation :: Token → Bool
isStation (TStation ) = True
isStation = False
fromStation :: Token → Station
fromStation (TStation x) = x
fromStation = error "fromStation"

-}

--token parsers
pTprodid :: Parser Token String
pTprodid = (\(Tprodid x) -> x) <$> satisfy checkType
    where checkType (Tprodid _) = True
          checkType _ = False

pStartCalendar :: Parser Token Token
pStartCalendar = satisfy checkType
    where checkType Tstartcal = True
          checkType _ = False

pEndCalendar :: Parser Token Token
pEndCalendar = satisfy checkType
    where checkType Tendcal = True
          checkType _ = False

pStartVEvent :: Parser Token Token
pStartVEvent = satisfy checkType
    where checkType Tstartevent = True
          checkType _ = False
          
pEndVEvent :: Parser Token Token
pEndVEvent = satisfy checkType
    where checkType Tendevent = True
          checkType _ = False

pTdtstamp :: Parser Token Token
pTdtstamp = satisfy checkType
    where checkType (Tdtstamp _) = True
          checkType _ = False

pTdtstart :: Parser Token Token
pTdtstart = satisfy checkType
    where checkType (Tdtstart _) = True
          checkType _ = False

pTdtend :: Parser Token Token
pTdtend = satisfy checkType
    where checkType (Tdtend _) = True
          checkType _ = False

pTuid :: Parser Token Token
pTuid = satisfy checkType
    where checkType (Tuid _) = True
          checkType _ = False

pTdescription :: Parser Token Token
pTdescription = satisfy checkType
    where checkType (Tdescription _) = True
          checkType _ = False

pTlocation :: Parser Token Token
pTlocation = satisfy checkType
    where checkType (Tlocation _) = True
          checkType _ = False

pTsummary :: Parser Token Token
pTsummary = satisfy checkType
    where checkType (Tsummary _) = True
          checkType _ = False

pTempty :: Parser Token Token
pTempty = satisfy checkType
    where checkType (Tempty) = True
          checkType _ = False

{-
pVEvent :: Parser Token VEvent
pVEvent = VEvent <$
                pStartVEvent <*>
                pTdtstamp <*> 
                pTuid <*> 
                pTdtstart <*> 
                pTdtend <*>
                optional pTdescription <*> 
                optional pTsummary <*>
                optional pTlocation <*
                pEndVEvent
            -}

pVEvents :: Parser Token [VEvent]
pVEvents = catMaybes <$> many pVEvent

makeEventChoice :: Parser Token Token
makeEventChoice = choice [   
                    pTdtstamp,
                    pTuid,
                    pTdtstart, 
                    pTdtend,
                    pTdescription,
                    pTsummary,
                    pTlocation
                ]

pVEvent :: Parser Token (Maybe VEvent)
pVEvent = pvevent <$ pStartVEvent <*> many makeEventChoice <* pEndVEvent
            where
                useProp [x] = Just x
                useProp [] = Nothing
                pvevent prop = do      
                    dtstamp     <- useProp [x | (Tdtstamp x) <- prop]
                    uid         <- useProp [x | (Tuid x) <- prop]
                    dtstart     <- useProp [x | (Tdtstart x) <- prop]
                    dtend       <- useProp [x | (Tdtend x) <- prop]
                    description <- Just $ useProp [x | (Tdescription x) <- prop]
                    summary     <- Just $ useProp [x | (Tsummary x) <- prop]
                    location    <- Just $ useProp [x | (Tlocation x) <- prop]
                    return (VEvent dtstamp uid dtstart dtend description summary location)


parseHeader :: Parser Token String
parseHeader = pTprodid <* pTempty <|> pTempty *> pTprodid

parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$ pStartCalendar <*> parseHeader <*> pVEvents <* pEndCalendar  

-- Exercise 2
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar fp = do 
                    h <- openFile fp ReadMode
                    hSetNewlineMode h noNewlineTranslation
                    str <- hGetContents h
                    return (recognizeCalendar str)

-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar (Calendar pid xs) =   printBeginCal       ++ printCrlf ++ 
                                    printVersion        ++ printCrlf ++ 
                                    printProdID ++ pid  ++ printCrlf ++ 
                                    concatMap printVEvent xs       ++  
                                    printEndCal 

printVEvent :: VEvent -> String
printVEvent (VEvent stamp uid s e d summ loc) = printBeginEvent                     ++ printCrlf ++
                                                printDtStamp ++ printDateTime stamp ++ printCrlf ++ 
                                                printDtStart ++ printDateTime s     ++ printCrlf ++ 
                                                printDtEnd   ++ printDateTime e     ++ printCrlf ++ 
                                                printUID     ++ uid                 ++ printCrlf ++ 
                                                printMaybe d    printDescription ++ 
                                                printMaybe summ printSummary ++ 
                                                printMaybe loc  printLocation ++
                                                printEndEvent ++ printCrlf


printMaybe :: (Maybe String) -> String -> String
printMaybe (Just x) p = p ++ x ++ printCrlf
printMaybe Nothing _ = ""

printDateTime :: DateTime -> String
printDateTime (DateTime (Date (Year y) (Month m) (Day d)) (Time (Hour h) (Minute mi) (Second s)) utc) 
    = f [y,m,d] ++ "T" ++ f [h,mi,s] ++ printUTC utc
        where f = concatMap showData

printUTC :: Bool -> String
printUTC True = "Z"
printUTC False = ""

--pretty print datetime's ints (Hour 1 -> "01")
showData :: (Show a) => a -> String
showData a = if null xs then "0" ++ [x] else o
    where o@(x:xs) = show a

-- Exercise 4
--count events
countEvents :: Calendar -> Int
countEvents = length . events

--find events
findEvents :: DateTime -> Calendar -> [VEvent]
findEvents dt (Calendar _ xs) = filter (checkEvent dt) xs

checkEvent :: DateTime -> VEvent -> Bool
checkEvent dt e = start <= dt && end > dt 
    where start = dtStart e
          end = dtEnd e

--checkoverlap
checkOverlapping :: Calendar -> Bool
checkOverlapping c = any (\cur-> findEvents (dtStart cur) c /= [] && findEvents (dtEnd cur) c /= []) (events c)

--time spent
timeSpent :: String -> Calendar -> Int
timeSpent s c = sum (map eventTime (findSummary s c))

maybeToString :: (Maybe String) -> String
maybeToString (Just x) = x
maybeToString Nothing = ""

--find events by summary
findSummary :: String -> Calendar -> [VEvent]
findSummary s (Calendar _ xs) = foldr (\c rs -> if (maybeToString (summary c)) == s then c:rs else rs) [] xs

--event duration time
eventTime :: VEvent -> Int
eventTime e = (((daysTillYearEnds bd) + sum (map daysInYear years) - (daysTillYearEnds ed)) * 24 + h) * 60 + m + (div s 60)
    where --startdate / enddate
          bd = date $ dtStart e
          ed = date $ dtEnd e
          --starttime / endtime
          bt = time $ dtStart e
          et = time $ dtEnd e
          --startyear / endyear
          by = unYear (year bd)
          ey = unYear (year ed)
          --year delta / min delta / sec delta
          h = unHour (hour bt) - unHour (hour et)
          m = unMinute (minute bt) - unMinute (minute et)
          s = unSecond (second bt) - unSecond (second et)
          years = if by == ey then [] else [by .. ey - 1]

daysTillYearEnds :: Date -> Int
daysTillYearEnds (Date y m (Day d)) = sum (map (flip daysInMonth' y) (xs (unMonth m))) - d
    where xs 12 = [12]
          xs x = [x..12]


-- Exercise 5

--ppCal datatypes
data EventWeek = EventWeek { eventdays :: [EventDay]}

data EventDay = EventDay {  unDate :: Date,
                            unEvents :: [VEvent] }



--alle benodigde functies voor het printen zijn gedefineerd, de laatste stap ontbreekt echter
ppMonth :: Year -> Month -> Calendar -> Doc
ppMonth y m c = undefined--text (concatMap printEventWeek (createCalendar c (Date y m (Day 1))))


--ppVEvent start - end time
ppVEventTime :: VEvent -> String
ppVEventTime e = ppTime start ++ " - " ++ ppTime end
    where ppTime dt = showData (unHour (hour dt)) ++ ":" ++ showData (unMinute (minute dt))
          start = time $ dtStart e
          end = time $ dtEnd e

ppEventDayContent :: Maybe VEvent -> String
ppEventDayContent Nothing = replicate 12 ' ' ++ "|"
ppEventDayContent (Just e) = ppVEventTime e ++ " |"

ppEventDayHeader :: EventDay -> String
ppEventDayHeader (EventDay d xs) = show cday ++ replicate 12 ' ' ++ "|"
    where cday = (unDay (day d))

ppDottedLine :: String
ppDottedLine = line 13 ++ concat (replicate 6 ("+" ++ line 13)) ++ "+"
    where line n = (replicate n '-')

createCalendar :: Calendar -> Date -> [EventWeek]
createCalendar c d = map (createEventWeek c) xs
    where xs = chunksOf 7 (createDateList d [1..(daysInMonth (month d) (year d))])

createEventWeek :: Calendar -> [Date] -> EventWeek
createEventWeek c xs = EventWeek $ map (findEventsDay c) xs

findEventsDay :: Calendar -> Date -> EventDay
findEventsDay (Calendar _ xs) d = EventDay (d) (foldr (\e as -> if date (dtStart e) == d then e : as else as) [] xs)

eventWeekHeight :: EventWeek -> Int
eventWeekHeight e = foldr (\c x -> x + length (unEvents c)) 0 (eventdays e)

createDateList :: Date -> [Int] -> [Date]
createDateList (Date (Year y) (Month m) (Day d)) ds = foldr (\c xs -> ((Date (Year y) (Month m) (Day c)):xs)) [] ds

--returns days in month (needs year for leap year check)   
daysInMonth' :: Int -> Year -> Int
daysInMonth' m y = daysInMonth (Month m) y                         
daysInMonth :: Month -> Year -> Int
daysInMonth (Month m) y = ds !! (m - 1)
    where ds = [31,febDays y,31,30,31,30,31,31,30,31,30,31]
daysInYear :: Int -> Int
daysInYear y
    | leapYear (Year y) == True = 366
    | otherwise = 365

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
