trait Ord[A]:
  def name: String
  def lt(x: A, y: A): Boolean
given Ord[Int] with
  def name = "Int"
  def lt(x: Int, y: Int) = x < y

object Even:
  def unapply(n: Int): Option[Int] = if n % 2 == 0 then Some(n / 2) else None

// extractor that needs a given
object Small:
  def unapply[A](x: A)(using o: Ord[A]): Option[A] = Some(x)

case class Box[A](a: A, b: A)

@main def Test =
  // guard
  val g1: [A] => (A, A) => String = [A] => {
    case (x, y) if x == y => "same"
    case _ => "different"
  }
  // guard using evidence from the expected context function type
  val g2: [A] => Ord[A] ?=> (A, A) => String = [A] => {
    case (x, y) if summon[Ord[A]].lt(x, y) => "lt"
    case _ => "ge"
  }
  // guard using evidence from a context-bound type, no bound on the literal
  val g3: [A: Ord] => Box[A] => String = [A] => {
    case Box(x, y) if summon[Ord[A]].lt(x, y) => "lt"
    case Box(_, _) => "ge"
  }
  // context bound on the literal, evidence comes first
  val g4: [A] => Ord[A] ?=> Box[A] => String = [A: Ord] => {
    case Box(x, y) if summon[Ord[A]].lt(x, y) => "lt"
    case _ => "ge"
  }
  // extractor
  val e1: [A] => Int => String = [A] => {
    case Even(h) => s"even, half $h"
    case n => s"odd $n"
  }
  // extractor needing a given in the pattern
  val e2: [A] => Ord[A] ?=> A => String = [A] => {
    case Small(x) => s"small $x"
  }
  // nested patterns and alternatives
  val n1: [A] => Option[Box[A]] => String = [A] => {
    case Some(Box(x, y)) if x == y => "pair"
    case Some(_) | None => "other"
  }
  // type test on an abstract type
  val t1: [A] => A => String = [A] => {
    case i: Int => s"int $i"
    case s: String => s"string $s"
    case _ => "other"
  }
  println(g1[Int](1, 1)); println(g1[Int](1, 2))
  println(g2[Int](1, 2)); println(g2[Int](2, 1))
  println(g3[Int](Box(1, 2))); println(g3[Int](Box(2, 1)))
  println(g4[Int](Box(1, 2))); println(g4[Int](Box(2, 1)))
  println(e1[String](4)); println(e1[String](3))
  println(e2[Int](5))
  println(n1[Int](Some(Box(1, 1)))); println(n1[Int](None))
  println(t1[Int](1)); println(t1[String]("a")); println(t1[Double](1.0))
