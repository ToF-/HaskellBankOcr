module BankOcr
where
import Data.Char (digitToInt)

convert :: [String] -> String
convert ["","",""] = []
convert ss         = 
    convertDigit (concat (map (take 3) ss)) : convert (map (drop 3) ss)
    where
        convertDigit " _ | ||_|" = '0'
        convertDigit "     |  |" = '1'
        convertDigit " _  _||_ " = '2'
        convertDigit " _  _| _|" = '3'
        convertDigit "   |_|  |" = '4'
        convertDigit " _ |_  _|" = '5'
        convertDigit " _ |_ |_|" = '6'
        convertDigit " _   |  |" = '7'
        convertDigit " _ |_||_|" = '8'
        convertDigit " _ |_| _|" = '9'
        convertDigit _ = '?'

checksum :: String -> Bool
checksum = (==0) . (`mod` 11) . sum . zipWith (*) [9,8..1] . map digitToInt 

process :: [String] -> [String]
process [] = []
process (l:l':l'':ls) = processOCR [l,l',l''] : process ls
    where processOCR :: [String] -> String
          processOCR ss = account ++ suffixes account
            where account = convert ss
                  suffixes a | elem '?' a = " ILL"
                             | not (checksum a) = " ERR"
                             | otherwise = ""
process _ = []



