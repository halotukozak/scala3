trait Ord[A]

def test =
  val f = [A] => { case x => x } // error: missing parameter type
  val g: [A] => Int = [A] => { case x => x } // error
  // The evidence parameter of a context bound comes before a case lambda,
  // but after the value parameter in the expected type
  val h: [A: Ord] => A => A = [A: Ord] => { case x => x } // error
