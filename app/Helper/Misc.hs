module Helper.Misc where

countIf :: (a -> Bool) -> [a] -> Int
countIf x = length . filter x

-- Safe Lookup
(!!?) :: [a] -> Int -> Maybe a
xs !!? n = if length xs > n then Just $ xs !! n else Nothing

-- Combine Boolean functions via AND
(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
x <&&> y = (&&) <$> x <*> y

-- Combine Boolean functions via OR
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
x <||> y = (||) <$> x <*> y

isBetween :: (Ord a) => a -> a -> a -> Bool
isBetween x y z = (x <= z) && (z <= y)

-- Least Significant Digit is first
bintodec :: String -> Int
bintodec [] = 0
bintodec (x : xs) = read [x] + 2 * bintodec xs
