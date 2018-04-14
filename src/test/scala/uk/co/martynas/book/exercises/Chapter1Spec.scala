package uk.co.martynas.book.exercises

import org.specs2.mutable.Specification

class Chapter1Spec extends Specification {
  import Chapter1._
  "test" should {
    "be true" in {
      test shouldEqual true
    }
  }

  "compose" should {
    "compose functions correctly" in {
      val pow = (v: Int) => v * v
      val str = (v: Int) => v.toString

      val func = compose(str, pow)

      func(5) shouldEqual "25"
    }
  }

  "fuse" should {
    val left = Some(5)
    val right = Some("Hello")

    "return tuple of a and b when both a and b are some" in {
      fuse(left, right) shouldEqual Some((5, "Hello"))
    }

    "return None when left is None" in {
      fuse(None, right) shouldEqual None
    }

    "return None when right is None" in {
      fuse(left, None) shouldEqual None
    }
  }

  "check" should {
    val even = (n: Int) => n % 2 == 0
    "be true when only odd numbers are present" in {
      check(Seq(2, 4))(even) shouldEqual true
    }

    "be true when only one element exists and its even" in {
      check(Seq(2))(even) shouldEqual true
    }

    "be true when list is empty" in {
      check(Nil)(even) shouldEqual true
    }

    "be false when only one element exists and its odd" in {
      check(Seq(1))(even) shouldEqual false
    }

    "be false when one odd element exists in the middle of the list" in {
      check(Seq(2, 4, 5, 6))(even) shouldEqual false
    }
  }

  "pair" should {
    "support pattern matching" in {
      val pair = new Pair(5, "Hello")

      val (left, right) = pair match {
        case Pair(l, r) => (l, r)
      }

      left shouldEqual 5
      right shouldEqual "Hello"
    }
  }

  "permutations" should {
    "return empty list of permutations for empty string" in {
      permutations("") shouldEqual Nil
    }

    "return single element for permutations of a string with single character" in {
      permutations("a") shouldEqual Seq("a")
    }

    "return different permutations for longer string" in {
      permutations("abc") shouldEqual Seq("abc", "acb", "bac", "bca", "cba", "cab").sorted
    }
  }
}
