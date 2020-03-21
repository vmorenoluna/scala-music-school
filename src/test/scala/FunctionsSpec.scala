import map.Functions._

class FunctionsSpec extends UnitSpec {

  "prefixes" should "return all proper prefixes of a list" in {
    val list: List[Int] = List(1,2,3,4,5)

    prefixes(list) should equal(
      List(List(1), List(1, 2), List(1, 2, 3), List(1, 2, 3, 4), List(1, 2, 3, 4, 5))
    )
  }

  "twice" should "apply a function f twice to its argument" in {
    twice[Int](n => n+1)(2) should equal(4)
    twice[Int](twice[Int](n => n+1))(2) should equal(6)
    twice[Int](twice[Int](twice[Int](n => n+1)))(2) should equal(10)
  }

  "power" should "apply a function f n times to its argument" in {
    power[Int](n => n+1, 2)(2) should equal(4)
    power[Int](n => n+1, 4)(2) should equal(6)
    power[Int](n => n+1, 8)(2) should equal(10)
  }

}