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
parseDateTime = DateTime <$> parseDate <* (symbol 'T') <*> parseTime <*> parseUTC

readStringTillEnd :: Parser Char String
readStringTillEnd = many $ satisfy (\x -> x /= '\n' && x /= '\r')

anyToken :: Parser Char Token
anyToken = Tstartcal    <$ token "BEGIN:VCALENDAR"                          <* newLine <|>
           Tempty       <$ token "VERSION:2.0"                              <* newLine <|>
           Tprodid      <$ token "PRODID:"          <*> readStringTillEnd   <* newLine <|>
           Tstartevent  <$ token "BEGIN:VEVENT"                             <* newLine <|> 
           Tdtstamp     <$ token "DTSTAMP:"         <*> parseDateTime       <* newLine <|> 
           Tdtstart     <$ token "DTSTART:"         <*> parseDateTime       <* newLine <|> 
           Tdtend       <$ token "DTEND:"           <*> parseDateTime       <* newLine <|> 
           Tuid         <$ token "UID:"             <*> readStringTillEnd   <* newLine <|>
           Tsummary     <$ token "SUMMARY:"         <*> readStringTillEnd   <* newLine <|>
           Tlocation    <$ token "LOCATION:"        <*> readStringTillEnd   <* newLine <|>
           Tdescription <$ token "DESCRIPTION:"     <*> readStringTillEnd   <* newLine <|>
           Tendevent    <$ token "END:VEVENT"                               <* newLine <|> 
           Tendcal      <$ token "END:VCALENDAR"    <|> 
           Tendcal      <$ token "END:VCALENDAR"                            <* newLine
           where newLine = token "\r\n"

scanCalendar :: Parser Char [Token]
scanCalendar = greedy anyToken <* eof

 --Parse token functions
numberGen :: [Int] -> Int
numberGen = foldl(\x y -> 10*x + y) 0

pInt :: Int -> Parser Char Int
pInt n = numberGen <$> sequence (replicate n newdigit)

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

pStartCalendar :: Parser Token Token
pStartCalendar = (\x -> x) <$> satisfy checkType
    where checkType Tstartcal = True
          checkType _ = False

pEndCalendar :: Parser Token Token
pEndCalendar = (\x -> x) <$> satisfy checkType
    where checkType Tendcal = True
          checkType _ = False

pStartVEvent :: Parser Token Token
pStartVEvent = (\x -> x) <$> satisfy checkType
    where checkType Tstartevent = True
          checkType _ = False
          
pEndVEvent :: Parser Token Token
pEndVEvent = (\x -> x) <$> satisfy checkType
    where checkType Tendevent = True
          checkType _ = False
          

pTprodid :: Parser Token String
pTprodid = (\(Tprodid x) -> x) <$> satisfy checkType
    where checkType (Tprodid _) = True
          checkType _ = False


pTdtstamp :: Parser Token Token
pTdtstamp = (\x -> x) <$> satisfy checkType
    where checkType (Tdtstamp _) = True
          checkType _ = False

pTdtstart :: Parser Token Token
pTdtstart = (\x -> x) <$> satisfy checkType
    where checkType (Tdtstart _) = True
          checkType _ = False

pTdtend :: Parser Token Token
pTdtend = (\x -> x) <$> satisfy checkType
    where checkType (Tdtend _) = True
          checkType _ = False

pTuid :: Parser Token Token
pTuid = (\x -> x) <$> satisfy checkType
    where checkType (Tuid _) = True
          checkType _ = False

pTdescription :: Parser Token Token
pTdescription = (\x -> x) <$> satisfy checkType
    where checkType (Tdescription _) = True
          checkType _ = False

pTlocation :: Parser Token Token
pTlocation = (\x -> x) <$> satisfy checkType
    where checkType (Tlocation _) = True
          checkType _ = False

pTsummary :: Parser Token Token
pTsummary = (\x -> x) <$> satisfy checkType
    where checkType (Tsummary _) = True
          checkType _ = False

pTempty :: Parser Token Token
pTempty = (\x -> x) <$> satisfy checkType
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
                pEndVEvent-}



aa :: Parser Token Token
aa = choice [   pTdtstamp,
                pTuid,
                pTdtstart, 
                pTdtend,
                pTdescription,
                pTsummary,
                pTlocation
                ]

pVEvent :: Parser Token (Maybe VEvent)
pVEvent = f <$> many aa
            where f props = do      
                    dtstamp  <- exactlyOnce[p | (Tdtstamp p) <- props]
                    uid      <-exactlyOnce [p | (Tuid p) <- props]
                    dtstart  <- exactlyOnce[p | (Tdtstart p) <- props]
                    dtend    <-exactlyOnce [p | (Tdtend p) <- props]
                    desc     <-zeroOrOnce [p | (Tdescription p) <- props]
                    summary  <- zeroOrOnce[p | (Tsummary p) <- props]
                    location <- zeroOrOnce [p | (Tlocation p) <- props]
                    return (VEvent dtstamp uid dtstart dtend desc summary location)
exactlyOnce :: [a] -> Maybe a
exactlyOnce []     = Nothing
exactlyOnce [x]    = Just x
exactlyOnce (x:xs) = Nothing

zeroOrOnce :: [a] -> Maybe (Maybe a)
zeroOrOnce []     = Just Nothing
zeroOrOnce [x]    = Just (Just x)
zeroOrOnce (x:xs) = Nothing

parseCalendar :: Parser Token Calendar
parseCalendar = a <|> b
    where a = Calendar <$ pStartCalendar <*> pTprodid <* pTempty <*> (catMaybes <$> many (pStartVEvent *> pVEvent <* pEndVEvent)) <* pEndCalendar
          b= Calendar <$ pStartCalendar <* pTempty <*> pTprodid <*> (catMaybes <$> many (pStartVEvent *> pVEvent <* pEndVEvent)) <* pEndCalendar
test = printCalendar $ fromJust $ Data.Text.Unsafe.inlinePerformIO $ readCalendar "examples/custom.ics"
test2 =  printCalendar $ fromJust $ Data.Text.Unsafe.inlinePerformIO $ readCalendar "examples/rooster_infotc.ics"

-- Exercise 2
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar fp = do 
                    h <- openFile fp ReadMode
                    hSetNewlineMode h noNewlineTranslation
                    str <- hGetContents h
                    return (recognizeCalendar str)
readCalendar2 :: FilePath -> IO (String)
readCalendar2 fp = do 
                    h <- openFile fp ReadMode
                    hSetNewlineMode h noNewlineTranslation
                    str <- hGetContents h
                    return (str)

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
countEvents = length . events

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents dt (Calendar _ xs) = foldr (\c ts -> (checkEvent dt c xs) ++ ts) [] xs

checkEvent :: DateTime -> VEvent -> [VEvent] -> [VEvent]
checkEvent dt e xs = if start < dt && end > dt then e:xs else xs
    where start = dtStart e
          end = dtEnd e


checkOverlapping :: Calendar -> Bool
checkOverlapping c = any (\cur-> if findEvents (dtStart cur) c /= [] && findEvents (dtEnd cur) c /= [] then True else False) (events c)


timeSpent :: String -> Calendar -> Int
timeSpent s (Calendar _ xs) = undefined

daysDif :: Date -> Date -> Int
daysDif x y = yearDif + (dayBegin - countDays y)
    where yearDif = sum (map daysInYear [(unYear (year x)) .. (unYear (year y))])
          dayBegin = daysInYear (unYear (year x)) - countDays x
          countDays (Date _ (Month 1) d) = unDay d
          countDays d = sum (map (flip daysInMonth' (year d)) [1..((unMonth (month d)) - 1)]) + unDay (day d)
data EventWeek = EventWeek { eventdays :: [EventDay]}

data EventDay = EventDay {  unDate :: Date }
                            --unEvents :: [VEvent] }

-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> Doc
ppMonth y m c = text "" --text (concatMap printEventWeek (createCalendar c (Date y m (Day 1))))
{-
printEventWeek = concatMap printEventDay [(EventDay (Date (Year 1) (Month 12) (Day 1))), 
                                            (EventDay (Date (Year 1) (Month 12) (Day 2))),
                                    (EventDay (Date (Year 1) (Month 12) (Day 3))),
                                    (EventDay (Date (Year 1) (Month 12) (Day 4))),
                                    (EventDay (Date (Year 1) (Month 12) (Day 5))),
                                    (EventDay (Date (Year 1) (Month 12) (Day 6))),
                                    (EventDay (Date (Year 1) (Month 12) (Day 7)))    
                                    ]
ppVEvent e = undefined-dtStart e dtEnd e
ppEventDay (EventDay d) = if mod cday 7 == 1 then printEDay else "| " ++ printEDay
    where cday = (unDay (day d))
          printEDay = show cday ++ replicate 13 ' '

ppDottedLine :: String
ppDottedLine = line 14 ++ concat (replicate 5 ("+" ++ line 15)) ++ line 15
    where line n = (replicate n '-')-}
{-
createCalendar :: Calendar -> Date -> [EventWeek]
createCalendar c d = map (createEventWeek c) xs
    where xs = chunksOf 7 (createDateList d [1..(daysInMonth (month d) (year d))])

createEventWeek :: Calendar -> [Date] -> EventWeek
createEventWeek c xs = EventWeek $ map (findEventsDay c) xs

findEventsDay :: Calendar -> Date -> EventDay
findEventsDay (Calendar _ xs) d = EventDay (d) (foldr (\e as -> if date (dtStart e) == d then e : as else as) [] xs)


createDateList :: Date -> [Int] -> [Date]
createDateList (Date (Year y) (Month m) (Day d)) ds = foldr (\c xs -> ((Date (Year y) (Month m) (Day c)):xs)) [] ds
-}
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
