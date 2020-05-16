package music

import music.Types.{Pitch, Volume => Vol}
import music.Music.mMap

sealed trait NoteAttribute

// TODO eq typeclass
final case class Volume(value: Int) extends NoteAttribute // 0-127
final case class Fingering(value: Int) extends NoteAttribute
final case class Dynamics(value: String) extends NoteAttribute
final case class Params(value: List[Double]) extends NoteAttribute

object Music1 {

  type Note1 = (Pitch, List[NoteAttribute])
  type Music1 = Music[Note1]

  sealed trait ToMusic1TypeClass[A] {
    def transform(m: Music[A]): Music1
  }

  implicit val ToMusic1PitchInstance: ToMusic1TypeClass[Pitch] = new ToMusic1TypeClass[Pitch] {
    override def transform(m: Music[Pitch]): Music1 =
      mMap[Pitch, Note1](p => (p, List.empty), m)
  }
  implicit val ToMusic1PitchVolumeInstance: ToMusic1TypeClass[(Pitch, Vol)] = new ToMusic1TypeClass[(Pitch, Vol)] {
    override def transform(m: Music[(Pitch, Vol)]): Music1 =
      mMap[(Pitch, Vol), Note1](a => (a._1, List(Volume(a._2))), m)
  }
  implicit val ToMusic1Note1Instance: ToMusic1TypeClass[Note1] = new ToMusic1TypeClass[Note1] {
    override def transform(m: Music[Note1]): Music1 = m
  }

  final implicit class Music1Ops[A](private val self: Music[A]) {
    def toMusic1()(implicit p: ToMusic1TypeClass[A]): Music1 = p.transform(self)
  }

}

