import map.Functions._

class FunctionsSpec extends UnitSpec {

  "prefixes" should "return all proper prefixes of a list" in {
    val list: List[Int] = List(1,2,3,4,5)

    prefixes(list) should equal(
      List(List(1), List(1, 2), List(1, 2, 3), List(1, 2, 3, 4), List(1, 2, 3, 4, 5))
    )
  }

}