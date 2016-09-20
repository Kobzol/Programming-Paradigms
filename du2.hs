module Du2 where

import Prelude

type Name = String
type Price = Integer
type BarCode = Integer
type Database = [(BarCode, Name, Price)]
type TillType = [BarCode]
type BillType = [(Name,Price)]
type GroupedBillType = (Name, [Price])

store = [(1, "Jablko", 15), (2, "Hruska", 18), (3, "Cokolada", 49), (4, "Jahodovy liker s ovocem", 168)]
till = [1, 1, 2, 2, 2, 3, 4, 4]

repeatItem :: a -> Int -> [a]
repeatItem a count = take count (repeat a)

makeBill :: Database -> TillType -> BillType
makeBill db till = map (\t -> head [(n, p) | (c, n, p) <- db, c == t]) till

padRight :: String -> Char -> Int -> String
padRight str c width = str ++ (repeatItem c (width - (length str)))

groupProducts :: BillType -> [GroupedBillType] -> [GroupedBillType]
groupProducts [] x = x
groupProducts ((n, p):xs) ys = groupProducts (filter (\(name, price) -> name /= n) xs) (grouped : ys)
  where
    grouped = (n, [price | (name, price) <- (n, p):xs, name == n])

sortProducts :: [GroupedBillType] -> [GroupedBillType]
sortProducts [] = []
sortProducts ((name, prices):xs) = sortProducts lhs ++ [(name, prices)] ++ sortProducts rhs
  where
    lhs = filter (\(n, p) -> sum p < sum prices) xs
    rhs = filter (\(n, p) -> sum p >= sum prices) xs

formatRow :: GroupedBillType -> String -> String
formatRow (name, prices) padding = name ++ padding ++ priceStr
  where
    priceStr = show (sum prices) ++ " Kc (" ++ "x" ++ show (length prices) ++ ")"

formatRowToWidth :: GroupedBillType -> Int -> String
formatRowToWidth b width = formatRow b (repeatItem '.' (width - length(formatRow b "")))

findWidth :: [GroupedBillType] -> Int
findWidth b = maximum (map (\x -> length (formatRow x ".....")) b)

formatPrice :: Integer -> Int -> String
formatPrice price width = padRight "Castka" '.' (width - length priceStr) ++ priceStr
  where
    priceStr = show price ++ " Kc"

formatBill :: BillType -> String
formatBill bill = concat (map (++ "\n")(
                    [padRight "Uctenka" ' ' width]
                    ++ repeatItem (padRight "" '-' width) 2
                    ++ map (\p -> formatRowToWidth p width) products
                    ++ [padRight "" '-' width]
                    ++ [formatPrice (sum [sum p | (name, p) <- products]) width]
                  ))
  where
    products = sortProducts (groupProducts bill [])
    width = findWidth products

produceBill :: Database -> TillType -> String
produceBill db till = formatBill (makeBill db till)
