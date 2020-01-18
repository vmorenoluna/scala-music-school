package chapter2

import spire.math.Rational

final object Types {
  type Octave = Int
  type Pitch = (PitchClass, Octave)
  type Duration = Rational
  type AbsPitch = Int
  type Tempo = Rational
  type PhraseAttribute = Any     // TODO
}
