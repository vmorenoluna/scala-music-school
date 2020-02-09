import Euterpea

main = play $ twoFiveOne (C,4) wn


minor :: Music Pitch -> Music Pitch 
minor root = root :=: transpose 3 root :=: transpose 7 root

major :: Music Pitch -> Music Pitch 
major root = root :=: transpose 4 root :=: transpose 7 root
    
twoFiveOne :: Pitch -> Dur -> Music Pitch 
twoFiveOne p d = let ii = minor(transpose 3 (note d p))
                     v = major(transpose 7 (note d p))
                     i = major(transpose 12 (tempo 0.5 (note d p)))
                 in ii :+: v :+: i
 
