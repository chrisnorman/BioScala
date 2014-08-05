/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package bioscala.gentypes

/**
 * Iteratee instances for routine consumption of simple patterns.
 * 
 */
object Iteratees {

  def takeOne[E]: Iteratee[E, Option[E]] = Continue {
    case in @ Element(e) => Done(Some(e), Pending)
    case in @ EndOfInput => Done(None, in)
    case Pending => takeOne
  }

  def take[E](n: Int): Iteratee[E, List[E]] = {
    def step(ct: Int, l: List[E]): Input[E] => Iteratee[E, List[E]] = {
      _ match {
        case Element(e) =>
          if (ct == 0) Done(l.reverse, Pending)
          else Continue(step(ct-1, e :: l))
        case EndOfInput => Done(l.reverse, EndOfInput)
      }
    }
    if (n == 0) Done(Nil, EndOfInput) else Continue(step(n, Nil))
  }

  def takeRight[E](n: Int): Iteratee[E, List[E]] = {
    def step(count: Int, l: List[E]): Input[E] => Iteratee[E, List[E]] = {
      _ match {
        case Element(e) =>
          if (count >= n) // TODO: this list management is inefficient....
            Continue(step(count+1, e :: l.take(n-1))) // keep the last n-1 chars
          else
            Continue(step(count+1, e :: l))
        case EndOfInput => Done(l.reverse, EndOfInput)
      }
    }
    if (n == 0) Error(new IllegalStateException("takeRight count must be > 0"))
    else Continue(step(0, Nil))
  }

  def takeWhile[E](p: E => Boolean, result: Seq[E] = IndexedSeq[E]()): Iteratee[E, Seq[E]] = 
    Continue {
	  case in @ Element(e) =>
	    if (p(e)) takeWhile(p, result :+ e)	      
	    else Done(result, in)
	  case in @ EndOfInput => Done(result, in)
	  case _ => takeWhile(p, result)
	}

  def takeUntil[E](p: E => Boolean, result: Seq[E] = IndexedSeq[E]()): Iteratee[E, Seq[E]] = 
    takeWhile(c => !p(c))

  def takeIf[E](p: E => Boolean): Iteratee[E, Option[E]] = {
    for {
  	  a <- peek[E]
  	  if (p(a))
  	  b <- takeOne
  	} yield(b)
  }

  /*
   * Take a line of (characters) as a String.
   */
  def takeLine: Iteratee[Char, String] = {
    for {
      a <- takeWhile[Char](c => (c != '\r') && (c != '\n'))
      _ <- takeWhile[Char](c => (c == '\r') || (c == '\n'))
    } yield a.mkString
  }
  
  def peek[E]: Iteratee[E, Option[E]] = {
    Continue {
      case in @ Element(e) => Done(Some(e), in)
      case in @ EndOfInput => Done(None, in)
      case Pending => peek
    }
  }

  def expect[E](e: E): Iteratee[E, Unit] = takeOne.flatMap {
    case Some(c) if c == e => Done(Unit, EndOfInput)
    case Some(c) => Error(new IllegalStateException("Expected: " + e.toString))
    case None => Error(new IllegalStateException("Expected: " + e.toString))
  }

  def dropWhile[E](p: E => Boolean): Iteratee[E, Unit] = {
    Continue {
      case in @ Element(e) if (!p(e)) => Done(Unit, in)
      case in @ EndOfInput => Done(Unit, in)
      case _ => dropWhile(p)
    }
  }
}
