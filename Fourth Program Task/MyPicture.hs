module MyPicture where
import Data.Char
import Data.List

a = ["     A      ",
    "    A A     ", 
    "   A   A    ",
    "  A     A   ",
    " AAAAAAAAA  ",
    "A         A "]


b = ["BBBBBBBBBBB ", 
    "BB        B ",
    "BBBBBBBBBB  ",
    "BBBBBBBBBB  ",
    "BB        B ",
    "BBBBBBBBBBB "]


c = [" CCCCCCCCC ", 
    "C          ",
    "C          ",
    "C          ",
    "C          ",
    " CCCCCCCCC "]


d = ["DDDDDDDDD   ", 
    "D        D  ",
    "D         D ",
    "D         D ",
    "D        D  ",
    "DDDDDDDDD   "]


e = ["EEEEEEEEEE ", 
    "E          ",
    "EEEEEEEEEE ",
    "EEEEEEEEEE ",
    "E          ",
    "EEEEEEEEEE "]


f = ["FFFFFFFFFF ", 
    "F          ",
    "FFFFFFFFFF ",
    "F          ",
    "F          ",
    "F          "]


g = [" GGGGGGGGGG ", 
    "G           ",
    "G           ",
    "G         G ",
    "G        GG ",
    " GGGGGGGG G "]


h = ["H       H ", 
    "H       H ",
    "HHHHHHHHH ",
    "HHHHHHHHH ",
    "H       H ",
    "H       H "]


i = ["  IIII ", 
    "   II  ",
    "   II  ",
    "   II  ",
    "   II  ",
    "  IIII "]


j = ["    JJJJ   ", 
    "     JJ    ",
    "     JJ    ",
    "     JJ    ",
    "   J JJ    ",
    "    JJJ    "]


k = ["    K  KKK ", 
    "    K KK   ",
    "    KKK    ",
    "    KKK    ",
    "    K KK   ",
    "    K  KKK "]


l = ["LL         ", 
    "LL         ",
    "LL         ",
    "LL         ",
    "LL         ",
    "LLLLLLLLL  "]


m = ["MMMM   MMMM ", 
    "M  M   M  M ",
    "M  M   M  M ",
    "M  M   M  M ",
    "M  M   M  M ",
    "M  MMMMM  M "]


n = ["NN     N ", 
    "N N    N ",
    "N  N   N ",
    "N   N  N ",
    "N    N N ",
    "N     NN "]


o = [" OOOOOOOOOO  ", 
    "O          O ",
    "O          O ",
    "O          O ",
    "O          O ",
    " OOOOOOOOOO  "]


p = ["PPPPPPPPP   ", 
    "P        P  ",
    "P        P  ",
    "PPPPPPPPP   ",
    "P           ",
    "P           "]


q = [" OOOOOOOOOO  ", 
    "O          O ",
    "O          O ",
    "O       OO O ",
    "O        OOO ",
    " OOOOOOOOO   "]


r = ["RRRRRRRRR   ", 
    "R        R  ",
    "RRRRRRRRR   ",
    "RR          ",
    "R R         ",
    "R  RRRRRRR  "]


s = [" SSSSSSSSS  ", 
    "S           ",
    "SSSSSSSSSS  ",
    "SSSSSSSSSSS ",
    "           S",
    "SSSSSSSSSSS "]


t = ["TTTTTTTT  ", 
    "   TT     ",
    "   TT     ",
    "   TT     ",
    "   TT     ",
    "   TT     "]


u = ["U         U ", 
    "U         U ",
    "U         U ",
    "U         U ",
    "U         U ",
    " UUUUUUUUU  "]


v = ["V         V  ", 
    " V       V   ",
    "  V     V    ",
    "   V   V     ",
    "    V V      ",
    "     V       "]


w = ["W     W     W ", 
    "W     W     W ",
    " W   W W   W  ",
    "  W W   W W   ",
    "   W     W    ",
    "   W     W    "]


x = ["XXXXX   XXXXX ", 
    "     X X      ",
    "      X       ",
    "       X      ",
    "      X X     ",
    "XXXXXX   XXXX "]


y = ["Y           Y ", 
    " YY       YY  ",
    "   YYYYYYY    ",
    "      Y       ",
    "      Y       ",
    "      Y       "]


z = ["ZZZZZZZZZZZZZ ", 
    "          ZZ  ",
    "        ZZZ   ",
    "   ZZZZZ      ",
    " ZZZ          ",
    "ZZZZZZZZZZZZZ "]


num0 = ["   000  ", 
    "  0   0 ",
    "  0   0 ",
    "  0   0 ",
    "  0   0 ",
    "   000  "]


num1 = ["   11   ", 
    "   11   ",
    "   11   ",
    "   11   ",
    "   11   ",
    "   11   "]


num2 = [" 2222222", 
    "2      2",
    "      2 ",
    "     2  ",
    "   22   ",
    "22222222"]


num3 = [" 3333333", 
    "3     3 ",
    "     3  ",
    "    33  ",
    " 3    33",
    " 3333333"]


num4 = ["4      4", 
    "4      4",
    "44444444",
    "       4",
    "       4",
    "       4"]


num5 = ["55555555 ", 
    "5        ",
    "5555555  ",
    " 5555555 ",
    "       5 ",
    "55555555 "]


num6 = ["66666666 ", 
    "6        ",
    "66666666 ",
    "6      6 ",
    "6      6 ",
    "66666666 "]


num7 = ["77777777 ", 
    "       7 ",
    "       7 ",
    "       7 ",
    "       7 ",
    "       7 "]


num8 = [" 888888  ", 
    "8      8 ",
    "88888888 ",
    "88888888 ",
    "8      8 ",
    " 888888  "]


num9 = [" 999999  ", 
    "9      9 ",
    "9      9 ",
    "99999999 ",
    "       9 ",
    "99999999 "]


space = ["        ", 
    "        ",
    "        ",
    "        ",
    "        ",
    "        "]

charToString :: Char -> [String]
charToString ch
    | ch == 'a' || ch == 'A' = a
    | ch == 'b' || ch == 'B' = b
    | ch == 'c' || ch == 'C' = c
    | ch == 'd' || ch == 'D' = d
    | ch == 'e' || ch == 'E' = e
    | ch == 'f' || ch == 'F' = f    
    | ch == 'g' || ch == 'G' = g
    | ch == 'h' || ch == 'H' = h
    | ch == 'i' || ch == 'I' = i
    | ch == 'j' || ch == 'J' = j
    | ch == 'k' || ch == 'K' = k
    | ch == 'l' || ch == 'L' = l
    | ch == 'm' || ch == 'M' = m
    | ch == 'n' || ch == 'N' = n
    | ch == 'o' || ch == 'O' = o
    | ch == 'p' || ch == 'P' = p
    | ch == 'q' || ch == 'Q' = q
    | ch == 'r' || ch == 'R' = r
    | ch == 's' || ch == 'S' = s
    | ch == 't' || ch == 'T' = t
    | ch == 'u' || ch == 'U' = u
    | ch == 'v' || ch == 'V' = v
    | ch == 'w' || ch == 'W' = w
    | ch == 'x' || ch == 'X' = x
    | ch == 'y' || ch == 'Y' = y
    | ch == 'z' || ch == 'Z' = z
    | ch == '0' = num0
    | ch == '1' = num1
    | ch == '2' = num2
    | ch == '3' = num3
    | ch == '4' = num4
    | ch == '5' = num5
    | ch == '6' = num6
    | ch == '7' = num7
    | ch == '8' = num8
    | ch == '9' = num9
    | ch == ' ' = space    
    | otherwise = ["undefine char to print", "undefine char to print", "undefine char to print", "undefine char to print", "undefine char to print", "undefine char to print"]
    

addTwoCh :: [String] -> [String] -> [String]
addTwoCh a b = zipWith (++) a b

zipAll :: [Char] -> [String] -> [String]
zipAll [] current = current
zipAll (x:xs) current = let zip = addTwoCh current (charToString x) 
                        in zipAll xs zip

empty = ["","","","","",""]

sayit :: String -> IO ()
sayit = putStr . say

say :: String -> String
say str = unlines (zipAll str empty)