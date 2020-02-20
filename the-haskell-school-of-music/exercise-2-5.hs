import Euterpea

transM :: AbsPitch -> Music Pitch -> Music Pitch
transM ap (Prim (Note d p)) = Prim (Note d (trans ap p))
transM ap (Prim (Rest d)) = Prim (Rest d)
transM ap (m1 :+: m2) = transM ap m1 :+: transM ap m2
transM ap (m1 :=: m2) = transM ap m1 :=: transM ap m2
transM ap (Modify (Transpose t) m) = transM (ap + t) m
transM ap (Modify x m) = Modify x (transM ap m)

m, mt :: Music Pitch
m = c 4 qn :+: d 4 qn :+: e 4 qn :+: f 4 qn
mt = transM 7 m

main = play $ m :+: mt