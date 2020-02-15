package chapter2

import chapter2.Types._
import org.specs2.mutable._

class exercises extends Specification {

  "absPitch (pitch ap)" should {
    "return ap" in {
      val ap: AbsPitch = 60
      absPitch(pitch(ap)) mustEqual ap
    }
  }

}
