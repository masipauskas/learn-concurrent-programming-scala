package uk.co.martynas.book.exercises

import java.util.concurrent.TimeUnit

import org.specs2.matcher.FutureMatchers
import org.specs2.mutable.Specification
import uk.co.martynas.book.exercises.Chapter4._

import scala.concurrent.{Await, Future, Promise, TimeoutException}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

class Chapter4Spec extends Specification with FutureMatchers {
  "IVar should" in {
    "allow set and get value" in {
      val ivar = new IVar[Int]
      ivar := 5
      ivar() shouldEqual 5
    }

    "throw exception when apply before set" in {
      val ivar = new IVar[Int]
      ivar() should throwA[TimeoutException]
    }

    "throw exception when multiple set" in {
      val ivar = new IVar[Int]
      ivar := 5
      ivar.:=(5) should throwA[IllegalStateException]
    }

    "not update the value on 2nd set" in {
      val ivar = new IVar[Int]
      ivar := 5
      ivar.:=(5) should throwA[IllegalStateException]
      ivar() shouldEqual 5
    }

    "get the same value with multiple apply" in {
      val ivar = new IVar[Int]
      ivar := 5
      ivar() shouldEqual 5
      ivar() shouldEqual 5
    }
  }

  "Future.exists should behave as expected" should {
    "return true when matches" in {
      val future = Future(5)
      await(future.exists(v => v % 5 == 0)) shouldEqual true
    }

    "return false when doesn't match" in {
      val future = Future(4)
      await(future.exists(v => v % 5 == 0)) shouldEqual false
    }

    "return false when exception" in {
      val future = Future[Int] {
        throw new IllegalArgumentException
      }

      await(future.exists(v => v % 5 == 0)) shouldEqual false
    }
  }

  "Future.existsPromise should behave as expected" should {
    "return true when matches" in {
      val future = Future(5)
      await(future.existsPromise(v => v % 5 == 0)) shouldEqual true
    }

    "return false when doesn't match" in {
      val future = Future(4)
      await(future.existsPromise(v => v % 5 == 0)) shouldEqual false
    }

    "return false when exception" in {
      val future = Future[Int] {
        throw new IllegalArgumentException
      }

      await(future.existsPromise(v => v % 5 == 0)) shouldEqual false
    }
  }

  "Future.existsAsync should behave as expected" should {
    "return true when matches" in {
      val future = Future(5)
      await(future.existsAsync(v => v % 5 == 0)) shouldEqual true
    }

    "return false when doesn't match" in {
      val future = Future(4)
      await(future.existsAsync(v => v % 5 == 0)) shouldEqual false
    }

    "return exception when exception" in {
      val future = Future[Int] {
        throw new IllegalArgumentException
      }

      await(future.existsAsync(v => v % 5 == 0)) should throwA[IllegalArgumentException]
    }
  }

  "IMAP" should {
    "should get value after update" in {
      val map = new IMap[Int, String]
      map.update(1, "Hello")
      await(map(1)) shouldEqual "Hello"
    }

    "should get value set after update has been subscribed" in {
      val map = new IMap[Int, String]
      val result = map(1)
      map.update(1, "Hello")
      await(result) shouldEqual "Hello"
    }

    "should be ok to get value after the value has already been resolved" in {
      val map = new IMap[Int, String]
      map.update(1, "Hello")
      await(map(1)) shouldEqual "Hello"
      await(map(1)) shouldEqual "Hello"
    }

    "should throw execption when trying to update second time" in {
      val map = new IMap[Int, String]
      map.update(1, "Hello")
      map.update(1, "Hello") should throwA[IllegalStateException]
    }
  }

  "Promise.compose" should {
    "compose functions correctly" in {
      val promise = Promise[Int]()
      val stringResult = promise.compose[String](v => v.toInt)

      stringResult.complete(Success("5"))

      await(promise.future) shouldEqual 5
    }


    "fail results correctly if initial result is failure" in {
      val promise = Promise[Int]()
      val stringResult = promise.compose[String](v => v.toInt)

      stringResult.complete(Failure(new IllegalStateException()))

      await(promise.future) should throwA[IllegalStateException]
    }

    "fail results correctly if composed function result is failure" in {
      val promise = Promise[Int]()
      val stringResult = promise.compose[String](v => throw new IllegalStateException())

      stringResult.complete(Success("5"))

      await(promise.future) should throwA[IllegalStateException]
    }
  }

  "spawn" should {
    val duration = Duration(2, TimeUnit.SECONDS)
    "return 0 for ls /" in {
      await(spawn("/bin/ls"), duration) shouldEqual 0
    }

    "return negative for command which doesn't exist" in {
      await(spawn("/usr/bin/false"), duration) shouldEqual 1
    }
  }

  "execute" should {
    val updateDuration = Duration(50, TimeUnit.MILLISECONDS)
    val timeout = Duration(2, TimeUnit.SECONDS)

    "not update if future is already returned" in {
      var value: Int = 0

      val result = execute(() => value = value + 1, updateDuration, timeout)(() =>5)
      result shouldEqual 5
      value shouldEqual 0
    }

    "update 3 times, if future blocked for 160 ms" in {
      var value: Int = 0

      val result = execute(() => value = value + 1, updateDuration, timeout) {() =>
        Thread.sleep(160)
        5
      }

      result shouldEqual 5
      value shouldEqual 3
    }

    "not update if future failed already" in {
      var value: Int = 0

      execute[Int](() => value = value + 1, updateDuration, timeout) { () =>
        throw new UnsupportedOperationException
      } should throwAn[UnsupportedOperationException]
      value shouldEqual 0
    }

    "time out after time out duration and not update if update is higher" in {
      var value: Int = 0

      execute[Int](() => value = value + 1, timeout, updateDuration) { () =>
        Thread.sleep(100)
        5
      } should throwAn[TimeoutException]
      value shouldEqual 0
    }
  }


  def await[T](future: Future[T], duration: Duration = Duration(20, TimeUnit.MILLISECONDS)): T = {
    Await.result(future, duration)
  }
}
