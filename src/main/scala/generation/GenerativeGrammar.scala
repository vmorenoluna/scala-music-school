package generation

import music.Music
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

  def gen[A](f: ReplFun[A], grammar: Grammar[A], seed: Int): LazyList[A] = {
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
    lazy val result: LazyList[(A, LazyList[Rand])] = xs #:: result.map(tuple => f(newRules)(tuple._1, tuple._2))
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

  private def checkProbs[A](rules: List[(Rule[A], Prob)]): Boolean = {
    val groupedRules: List[List[(Rule[A], Prob)]] = rules.groupBy(_._1.lhs).values.toList
    groupedRules.forall(
      checkSum(_) == true
    )
  }

  private def checkSum[A](rules: List[(Rule[A], Prob)]): Boolean = {
    val eps = 0.001
    val mySum = rules.map(_._2).sum
    Math.abs(1.0 - mySum) <= eps
  }

  private def sameLHS[A](t1: (Rule[A], Prob), t2: (Rule[A], Prob)): Boolean =
    t1._1.lhs == t2._1.lhs // TODO eq type class

}

object MusicGrammar {

  sealed trait LSys[+A]

  final case class N[A](symbol: A) extends LSys[A]

  final case class :+[A](m: LSys[A], n: LSys[A]) extends LSys[A]

  final case class ::[A](m: LSys[A], n: LSys[A]) extends LSys[A]

  final case class Id[A]() extends LSys[A]

  sealed trait LFun
  final case class Inc() extends LFun
  final case class Dec() extends LFun
  final case class Same() extends LFun

  import generation.GenerativeGrammar.{Prob, Rule, Rand}

  def replFun(rules: List[List[(Rule[LSys[LFun]], Prob)]])(s: LSys[LFun], rands: LazyList[Rand]): (LSys[LFun], LazyList[Rand]) =
    s match {
      case a :+ b => {
        val (a1, rands1) = replFun(rules)(a, rands)
        val (b1, rands2) = replFun(rules)(b, rands1)
        (:+(a1, b1), rands2)
      }
      case a :: b => {
        val (a1, rands1) = replFun(rules)(a, rands)
        val (b1, rands2) = replFun(rules)(b, rands1)
        (::(a1, b1), rands2)
      }
      case Id() => (Id(), rands)
      case N(x) => (getNewRHS(rules, N(x), rands.head), rands.tail)
    }

  private def getNewRHS[A](rrs: List[List[(Rule[A], Prob)]], ls: A, rand: Rand): A =
    rrs.find(l => l.head._1.lhs == ls) match {
      case Some(rs) => loop(rs, rand)
      case None => throw new Exception("Remove this exception in getNewRHS method") // TODO
    }

  private def loop[A](rs: List[(Rule[A], Prob)], rand: Rand): A = rs match {
    case (r, p) +: rs => if (rand <= p) r.rhs else loop(rs, rand)
    case Nil => throw new Exception("Remove this exception in loop method") // TODO
  }

  // interpretation rules
  type IR[A, B] = List[(A, Music[B] => Music[B])]

  def interpret[A, B](lsys: LSys[A], r: IR[A, B], m: Music[B]): Music[B] =
    lsys match {
      case ::(a, b) => interpret(a, r, interpret(b, r, m))
      case :+(a, b) => interpret(a, r, m) :+: interpret(b, r, m)
      case Id() => m
      case N(x) => r.find(ir => ir._1 == x) match {
        case Some(ir) => ir._2(m)
        case None => throw new Exception("Remove this exception in interpret method") // TODO
      }
    }

}