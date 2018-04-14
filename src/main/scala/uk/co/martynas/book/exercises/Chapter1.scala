package uk.co.martynas.book.exercises

import scala.annotation.tailrec

object Chapter1 {
  def test: Boolean = true

  def compose[A, B, C](g: B => C, f: A => B): A => C = (v: A) => g(f(v))

  def fuse[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = for {
    left <- a
    right <- b
  } yield (left, right)

  @tailrec
  def check[T](xs: Seq[T])(pred: T => Boolean): Boolean = xs match {
    case Nil => true
    case head :: rest => pred(head) && check(rest)(pred)
  }

  class Pair[P, Q](val first: P, val second: Q)
  object Pair {
    def unapply[P, Q](arg: Pair[P, Q]): Option[(P, Q)] = Some((arg.first, arg.second))
  }

  def permutations(x: String): Seq[String] = {
    def perms(letters: Seq[Char], n: Int): Seq[Seq[Char]] = {
      if (n == 1) for { l <- letters } yield Seq(l)
      else {
        val result = for {
          l <- letters
          p <- perms(letters, n - 1) if !p.contains(l)
        } yield l +: p

        result.distinct
      }
    }

    perms(x.distinct, x.distinct.length).map(_.mkString("")).sorted
  }

  /*
    Exercise 6.
    I think the general approach, would be to try to use concept of quarum. I.e. if at least half of remaining colleagues
    agree with proposed time - then you are done - and assume that's the decided time.
    Hence algorithm would be something along the lines of:

    Case -> Propose Time ->
      1. Pick a time. Send message for everyone with it.

      2. Receive time proposal. If works send a response to everyone that you agree with a specific time.

      3. Receive time response.
        If you agree with time proposal (or you sent it) and you receive half or more agreements from all others.
        Assume that time was agreed.

    Exercise 7.
    The same as above - with whiteboard. When you walk by - you can see a proposed time. You can either:
    1. agree, and add checkbox to the agreement.
    2. propose new time, if less than half of the collegues yet agreed to the prior time.
    3. When you see that half of your collegues agreed  to the time - you can assume that everyone is going there.
   */
}
