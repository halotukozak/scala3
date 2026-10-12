trait Ord[A]:
  def name: String
given Ord[Int] with
  def name = "Int"

@main def Test =
  val f1: [A] => A => A = [A] => { case x => x }
  val f2: [A] => Ord[A] ?=> A => String = [A] => { case x => summon[Ord[A]].name + x }
  val f3: [A: Ord] => A => String = [A] => { case x => summon[Ord[A]].name + x }
  val f4: [A] => (A, A) => (A, A) = [A] => { case (x, y) => (y, x) }
  val f5: [A: Ord] => Option[A] => String = [A] => {
    case Some(x) => summon[Ord[A]].name + x
    case None => "none"
  }
  val f6: [A] => Ord[A] ?=> A => String = [A: Ord] => { case x => summon[Ord[A]].name + x }
  println(f1[Int](1))
  println(f2[Int](2))
  println(f3[Int](3))
  println(f4[Int](4, 5))
  println(f5[Int](Some(6)))
  println(f5[Int](None))
  println(f6[Int](7))
