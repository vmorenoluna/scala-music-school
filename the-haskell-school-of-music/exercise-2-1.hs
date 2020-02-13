import Euterpea

main = play $ t251 :+: wnr :+: twoFiveOne (C,4) wn

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in dMinor :+: gMajor :+: cMajor

minor :: Music Pitch -> Music Pitch 
minor root = root :=: transpose 3 root :=: transpose 7 root

major :: Music Pitch -> Music Pitch 
major root = root :=: transpose 4 root :=: transpose 7 root
    
twoFiveOne :: Pitch -> Dur -> Music Pitch 
twoFiveOne p d = let ii = minor(transpose 2 (note d p))
                     v = major(transpose 7 (note d p))
                     i = major(tempo 0.5 (note d p))
                 in ii :+: v :+: i
 
