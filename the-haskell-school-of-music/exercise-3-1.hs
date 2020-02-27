import Euterpea

f1 :: Int -> [Pitch] -> [Pitch]
f1 n ps = map (trans n) ps

f2 :: [Dur] -> [Music a]
f2 ds = map rest ds

f3 :: [Music Pitch] -> [Music Pitch]
f3 [] = []
f3 (m : ms) = staccato(m) : (f3 ms)

staccato :: Music Pitch -> Music Pitch
staccato (Prim (Note d p)) = Prim(Note (d/2) p) :+: Prim(Rest (d/2))
staccato (Prim (Rest d)) = Prim (Rest d)
staccato (m1 :+: m2) = staccato(m1) :+: staccato(m2)
staccato (m1 :=: m2) = staccato(m1) :=: staccato(m2)
staccato (Modify controller m) = Modify controller (staccato m)

music_normal = [c 4 qn , d 4 qn, e 4 qn, f 4 qn, g 4 qn, a 4 qn, b 4 qn, c 5 qn]
music_staccato = f3 music_normal

main = play $ (line music_normal) :+: hnr :+: (line music_staccato)