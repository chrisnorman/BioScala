/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.gentypes

/**
 * 
 */
object Iteratees {

  def take[E](n: Int): Iteratee[E, List[E]] = {
    def step(ct: Int, l: List[E]): Input[E] => Iteratee[E, List[E]] = {
      _ match {
        case Element(e) =>
          if (ct == 0)
            Done(l.reverse, Element(e))
          else
            Continue(step(ct-1, e :: l))
        case EndOfInput => Done(l.reverse, EndOfInput)
      }
    }
    if (n == 0) Done(Nil, EndOfInput) else Continue(step(n, Nil))
  }

  def takeRight[E](n: Int): Iteratee[E, List[E]] = {
    def step(count: Int, l: List[E]): Input[E] => Iteratee[E, List[E]] = {
      _ match {
        case Element(e) =>
          if (count >= n)
            // TODO: this list mgmt is inefficient....
            Continue(step(count+1, e :: l.take(n-1))) // keep the last n-1 chars
          else
            Continue(step(count+1, e :: l))
        case EndOfInput => Done(l.reverse, EndOfInput)
      }
    }
    if (n == 0) Error(new IllegalStateException("takeRight count must be > 0")) else Continue(step(0, Nil))
  }

  def takeWhile[E](p: E => Boolean, data: Seq[E] = IndexedSeq[E]()): Iteratee[E, Seq[E]] = 
    Continue {
	  case in @ Element(e) =>
	    if (p(e)) takeWhile(p, data :+ e)
	    else  Done(data, in)
	  case in @ EndOfInput => Done(data, in)
	  case _ => takeWhile(p, data)
	}

  def peek[E]: Iteratee[E, Option[E]] = {
    Continue {
      case in @ Element(e) => Done(Some(e), in)
      case in @ EndOfInput => Done(None, in)
      case Pending => peek
    }
  }

  def dropWhile[E](p: E => Boolean): Iteratee[E, Unit] = {
    Continue {
      case in @ Element(e) if (!p(e)) => Done(Unit, in)
      case in @ EndOfInput => Done(Unit, in)
      case _ => dropWhile(p)
    }
  }
}
