boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"

simple x y z = x * (y + z)

applyAll :: [a -> a] -> a -> a
applyAll fs = foldr (.) id fs

test = (applyAll [simple 2 2, (+3)] 5) == 20

main = putStrLn (boolToString test)





