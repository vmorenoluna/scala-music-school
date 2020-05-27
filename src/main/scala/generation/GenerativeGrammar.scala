package generation

import scala.collection.MapView
import scala.util.Random

object GenerativeGrammar {

  final case class Grammar[A](start: A, rules: Rules[A])

  sealed trait Rules[A]

  final case class Uni[A](rules: List[Rule[A]]) extends Rules[A]

  final case class Sto[A](rules: List[(Rule[A], Prob)]) extends Rules[A]

  final case class Rule[A](lhs: A, rhs: A)

  type Prob = Double

  type Rand = Double
  type ReplFun[A] = List[List[(Rule[A], Prob)]] => (A, LazyList[Rand]) => (A, LazyList[Rand])

  private def gen[A](f: ReplFun[A], grammar: Grammar[A], seed: Int): LazyList[A] = {
    val newRules: Sto[A] = toStoRules(grammar.rules)
    val rand = new Random(seed)
    val rands = LazyList.continually(rand.nextDouble())
    if (checkProbs(newRules.rules))
      generate(f, newRules, (grammar.start, rands))
    else {
      println("Stochastic rule-set is malformed.")
      LazyList.empty
    }
  }

  private def generate[A](f: ReplFun[A], rules: Sto[A], xs: (A, LazyList[Rand])): LazyList[A] = {
    val lists: List[List[(Rule[A], Rand)]] = rules.rules.groupBy(_._1.lhs).values.toList
    val newRules: List[List[(Rule[A], Prob)]] = lists.map(probDist)
    // https://github.com/scala/bug/issues/9909#issuecomment-292441829
    lazy val result : LazyList[(A, LazyList[Rand])] = xs #:: result.map(tuple => f(newRules)(tuple._1, tuple._2))
    result.map(_._1)
  }

  private def probDist[A](list: List[(Rule[A], Prob)]): List[(Rule[A], Prob)] = {
    val unzipped: (List[Rule[A]], List[Rand]) = list.unzip
    val newRules: List[(Rule[A], Prob)] = unzipped._1.zip(
      unzipped._2.scanLeft(0.0)(_ + _).tail
    )
    newRules
  }

  private def toStoRules[A](rules: Rules[A]): Sto[A] = rules match {
    case Sto(rs) => Sto(rs)
    case Uni(rs) => {
      val groupedRs: Map[A, List[Rule[A]]] = rs.groupBy(_.lhs)
      val groupedRsWithProb: MapView[A, List[(Rule[A], Prob)]] = groupedRs.view.mapValues(insertProb)
      Sto(
        groupedRsWithProb.values.toList.flatten
      )
    }
  }

  private def insertProb[A](rules: List[Rule[A]]): List[(Rule[A], Prob)] = {
    val prb: Prob = 1.0 / rules.length
    rules.map(a => (a, prb))
  }

  private def checkProbs[A](rules: List[(Rule[A], Prob)]): Boolean =
    checkSum(
      rules.groupBy(_._1.lhs).values.toList.flatten
    ) == true

  private def checkSum[A](rules: List[(Rule[A], Prob)]): Boolean = {
    val eps = 0.001
    val mySum = rules.map(_._2).sum
    Math.abs(1.0 - mySum) <= eps
  }

  private def sameLHS[A](t1: (Rule[A], Prob), t2: (Rule[A], Prob)): Boolean =
    t1._1.lhs == t2._1.lhs  // TODO eq type class



}
