boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"

simple x y z = x * (y + z)

applyEach :: [a -> b] -> a -> [b]
applyEach fs x = map ($ x) fs

test = (applyEach [simple 2 2, (+3)] 5) == [14, 8]

main = putStrLn (boolToString test)