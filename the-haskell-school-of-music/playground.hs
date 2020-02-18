import Euterpea

main = play $ (d 4 qn) :+: transpose 3 (note qn (C,4))

fj0, fj1, fj2, fj3, fj4 :: Music Pitch
fj0 = c 4 qn :+: c 4 qn :+: c 4 qn
fj1 = c 4 qn :+: d 4 qn :+: e 4 qn :+: c 4 qn
fj2 = e 4 qn :+: f 4 qn :+: g 4 hn
fj3 = g 4 en :+: a 4 en :+: g 4 en :+: f 4 en :+: e 4 qn :+: c 4 qn
fj4 = c 4 qn :+: g 3 qn :+: c 4 hn

fj :: Music Pitch
fj = fj0 :+: fj1 :+: fj1 :+: fj2 :+: fj2 :+: fj3 :+: fj3 :+: fj4


