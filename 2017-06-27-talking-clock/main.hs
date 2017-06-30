import System.Environment
import Data.List (intersperse)

--
main :: IO ()
main = do
    args <- getArgs
    if length args >= 1
       then putStrLn (concat (intersperse "\n" (map printTime args)))
       else putStrLn "Sorry, you need to pass a time"

-------------------------
--Time formatting stuff--
-------------------------

printTime :: [Char] -> [Char]
printTime [a,b,':','0','0'] = "It's " ++ (hoursFromString [a,b]) ++ " o'clock" ++ (getMorningAfternoonDescriptor [a,b])
printTime [a,b,':','1','5'] = "It's quarter past " ++ (hoursFromString [a,b]) ++ (getMorningAfternoonDescriptor [a,b])
printTime [a,b,':','3','0'] = "It's half past "    ++ (hoursFromString [a,b]) ++ (getMorningAfternoonDescriptor [a,b])
printTime [a,b,':','4','5'] = "It's quarter to "   ++ (hoursFromString [a,b]) ++ (getMorningAfternoonDescriptor [a,b])
printTime [a,b,':',c,d]     = 
    "It's " ++ (hoursFromString (a:b:[])) ++ (tensOfMinutesFromString c) ++ 
        (if (read (c:d:[])) < 20
           then individualMinutesFromString (c:d:[])
           else individualMinutesFromString [d])
       ++ getMorningAfternoon [a,b]
printTime _ = "Sorry, you need to pass in a valid time"

--------------------
--Hours Conversion--
--------------------

hoursFromNum :: Int -> [Char]
hoursFromNum i
  | h == 0  = "twelve"
  | h == 1  = "one"
  | h == 2  = "two"
  | h == 3  = "three"
  | h == 4  = "four"
  | h == 5  = "five"
  | h == 6  = "six"
  | h == 7  = "seven"
  | h == 8  = "eight"
  | h == 9  = "nine"
  | h == 10 = "ten"
  | h == 11 = "eleven"
  where
    h = if i >= 12
      then i -  12
      else i

nextHourFromString :: [Char] -> [Char]
nextHourFromString a
  | n == 23   = hoursFromNum 0
  | otherwise = hoursFromNum (n+1)
  where
    n = read a

hoursFromString :: [Char] -> [Char]
hoursFromString u = hoursFromNum (read u)

---------------------
--Minute Conversion--
---------------------

tensOfMinutesFromString :: Char -> [Char]
tensOfMinutesFromString u
  | t == 0 = " oh"
  | t == 2 = " twenty"
  | t == 3 = " thirty"
  | t == 4 = " fourty"
  | t == 5 = " fifty"
  | otherwise = ""
  where 
    t = read [u]

individualMinutesFromString :: [Char] -> [Char]
individualMinutesFromString u
  | t == 0  =  ""
  | t == 1  =  " one"
  | t == 2  =  " two"  
  | t == 3  =  " three"  
  | t == 4  =  " four"  
  | t == 5  =  " five"  
  | t == 6  =  " six"  
  | t == 7  =  " seven"  
  | t == 8  =  " eight"  
  | t == 9  =  " nine"  
  | t == 10 = " ten"  
  | t == 11 = " eleven"  
  | t == 12 = " twelve"  
  | t == 13 = " thirteen"  
  | t == 14 = " fourteen"  
  | t == 15 = " fifteen"  
  | t == 16 = " sixteen"  
  | t == 17 = " seventeen"  
  | t == 16 = " eighteen"  
  | t == 19 = " nineteen"
  where
      t = read (u)

---------------------------------
--Morning / Afternoon Indicator--
---------------------------------

getMorningAfternoon :: [Char] -> [Char]
getMorningAfternoon u
  | t > 12    = " am"
  | otherwise = " pm"
  where
    t = read(u) 

getMorningAfternoonDescriptor :: [Char] -> [Char]
getMorningAfternoonDescriptor u
  | t > 21 = " at night"
  | t > 18 = " in the evening"
  | t > 12 = " in the afternoon"
  | otherwise = " in the morning"
  where
    t = read u
