module HaskellStore where
import Text.Printf

-- name, amount and price per unit of the item
type Name = String -- name of the item
type Amount = Float -- amount, like kg or number
type Price = Float  -- price per unit
type Sum = Float

-- Items, Item
type Item = (Name, Amount, Price)
type Items = [Item]

type ItemWithSum = (Name, Amount, Price, Sum)
type ItemsWithSum = [ItemWithSum]

-- let header = ["Name    Amout    Price    Sum"]

addSum :: Items -> ItemsWithSum -> ItemsWithSum
addSum [] start = start
addSum ((n, a, p):xs) start = addSum xs start++[(n, a, p, a * p)]

fst (a,_,_,_) = a
snd' (_,a,_,_) = a
thd (_,_,a,_) = a
fth (_,_,_,a) = a

total :: ItemsWithSum -> ItemWithSum
total items = ("Total", sum [snd' x | x <- items], sum [thd x | x <- items], sum [fth x | x <- items])

showPre :: Float -> String
showPre x = printf "    %.2f" x

itemToString :: ItemWithSum -> String
itemToString (n, a, p, s) = n ++ (showPre a) ++ (showPre p) ++ (showPre s)

itemsToString :: ItemsWithSum -> [String] -> [String]
itemsToString [] current = current
itemsToString (item:items) current = itemsToString items current ++ [(itemToString item)]

printItems :: Items -> IO()
printItems items = putStr (unlines (itemsToString (addSum items []) ["Name    Amout    Price    Sum"]) ++ (itemToString (total (addSum items []))++"\n"))