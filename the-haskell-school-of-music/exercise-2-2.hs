import Euterpea

main = play $ fromBlues(
                         b0 :+: b1 :+: b2 :+: b3 :+: b4 :+:
                        ((b0 :+: b2 :+: b2 :+: b4) :=: (b3 :+: b3)) :+:
                         b5 :+: ((b0 :+: b0) :=: b3) :+:
                         (b1 :=: b0) :+:
                         b0 :+: b4
                       )

data BluesPitchClass = Ro | MT | Fo | Fi | MS

type BluesPitch = (BluesPitchClass, Octave)

ro :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro, o)
mt :: Octave -> Dur -> Music BluesPitch
mt o d = note d (MT, o)
fo :: Octave -> Dur -> Music BluesPitch
fo o d = note d (Fo, o)
fi :: Octave -> Dur -> Music BluesPitch
fi o d = note d (Fi, o)
ms :: Octave -> Dur -> Music BluesPitch
ms o d = note d (MS, o)

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues(Prim(Note d (Ro, o))) = Prim(Note d (C, o))
fromBlues(Prim(Note d (MT, o))) = Prim(Note d (Ef, o))
fromBlues(Prim(Note d (Fo, o))) = Prim(Note d (F, o))
fromBlues(Prim(Note d (Fi, o))) = Prim(Note d (G, o))
fromBlues(Prim(Note d (MS, o))) = Prim(Note d (Bf, o))
fromBlues(m1 :+: m2) = fromBlues m1 :+: fromBlues m2
fromBlues(m1 :=: m2) = fromBlues m1 :=: fromBlues m2
fromBlues(Modify x m) = Modify x (fromBlues m)

b0, b1, b2, b3, b4, b5 :: Music BluesPitch
b0 = ro 4 qn :+: mt 4 qn :+: fo 4 qn
b1 = mt 4 qn :+: fi 4 qn :+: ro 4 qn :+: fi 4 qn
b2 = ms 4 qn :+: fo 4 qn :+: mt 4 hn
b3 = mt 4 en :+: fo 4 en :+: fo 4 en :+: ro 4 en :+: ro 4 qn :+: mt 4 qn
b4 = fi 4 qn :+: mt 3 qn :+: ro 4 hn
b5 = ro 4 en :+: ro 4 qn :+: ro 4 en :+: mt 4 en :+: fo 4 qn :+: mt 4 qn