package cmn397.bioscala.gentypes

object Iteratees {

  def take[E](n: Int): Iteratee[E, List[E]] = {
    def step(ct: Int, l: List[E]): Input[E] => Iteratee[E, List[E]] = {
      _ match {
        case Element(e) => Continue(step(ct-1, e :: l))
        case EndOfInput => Done(l.reverse, EndOfInput)
      }
    }
    if (n == 0) Done(Nil, EndOfInput) else Continue(step(n, Nil))
  }

  def takeRight[E](n: Int): Iteratee[E, List[E]] = {
    def step(ct: Int, l: List[E]): Input[E] => Iteratee[E, List[E]] = {
      _ match {
        case Element(e) => Continue(step(ct-1, e :: l))
        case EndOfInput => Done(l.reverse, EndOfInput)
      }
    }
    if (n == 0) Done(Nil, EndOfInput) else Continue(step(n, Nil))
  }

  // takewhile
}
