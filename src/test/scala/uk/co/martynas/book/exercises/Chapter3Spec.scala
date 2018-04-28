package uk.co.martynas.book.exercises

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import org.specs2.mutable.Specification
import uk.co.martynas.book.exercises.Chapter3._

import scala.collection.mutable
import scala.compat.Platform.ConcurrentModificationException
import scala.util.Random

class Chapter3Spec extends Specification {
  "PiggyBackContext" should {
    val ctx = PiggyBackContext
    "execute in the same thread as caller" in {
      var result = 0
      val runner = new Runnable {
        override def run(): Unit = {
          Thread.sleep(200)
          result = 10
        }
      }

      ctx.execute(runner)
      result shouldEqual 10

    }

    "propagate exception to caller" in {
      val runner = new Runnable {
        override def run(): Unit = throw new IllegalArgumentException
      }

      ctx.execute(runner) should throwA[IllegalArgumentException]
    }
  }

  "TreiberStack" should {
    val stack = new TreiberStack[Int]

    "pop throw an exception when stack is empty" in {
      val stack = new TreiberStack[Int]

      stack.pop() should throwA[UnsupportedOperationException]
    }

    "push and pop should return correct value" in {
      stack.push(10)
      stack.pop() shouldEqual 10
    }

    "should concurrently push 2000 items and pop 2000 items correctly" in {
      val initial = (1 to 2000).toSet
      val results = new ConcurrentSkipListSet[Int]
      val latch = new CountDownLatch(2000)
      val tasks = initial.map(v => new Runnable {
        override def run(): Unit = {
          stack.push(v)
          results.add(stack.pop())
          latch.countDown()
        }
      }).par

      val executor = Executors.newFixedThreadPool(20)
      tasks.foreach(executor.execute)

      latch.await(500, TimeUnit.MILLISECONDS)
      results.toArray().toSet shouldEqual initial
    }
  }

  "LazyCell" should {
    "initialize value from constructor" in {
      var initialized = false
      val cell = new LazyCell[Int]({
        initialized = true
        10
      })

      initialized shouldEqual false
      cell.apply() shouldEqual 10
      initialized shouldEqual true
    }

    "call constructor only once" in {
      val counter = new AtomicInteger(0)
      val cell = new LazyCell[Int]({
        counter.incrementAndGet()
        10
      })

      val result = (1 to 10).par.map(_ => cell.apply()).seq.distinct.head
      result shouldEqual 10
      counter.get() shouldEqual 1
    }
  }

  "PureLazyCell" should {
    "initialize value from constructor" in {
      var initialized = false
      val cell = new PureLazyCell[Int]({
        initialized = true
        10
      })

      initialized shouldEqual false
      cell.apply() shouldEqual 10
      initialized shouldEqual true
    }

    "call constructor only once" in {
      val counter = new AtomicInteger(0)
      val cell = new PureLazyCell[Int]({
        counter.incrementAndGet()
        Thread.sleep(50)
        10
      })

      val result = (1 to 10).par.map(_ => cell.apply()).seq.distinct.head
      result shouldEqual 10
      counter.get() should beLessThan(10)
    }
  }

  "SyncConcurrentMap" should {
    "putIfAbsent when absent should add new value" in {
      val map = new SyncConcurrentMap[Int, String]

      map.putIfAbsent(1, "one") shouldEqual None
      map.get(1) shouldEqual Some("one")
    }

    "putIfAbsent when exists should leave value in tact" in {
      val map = new SyncConcurrentMap[Int, String]

      map.putIfAbsent(1, "one") shouldEqual None
      map.putIfAbsent(1, "two") shouldEqual Some("one")
      map.get(1) shouldEqual Some("one")
    }

    "remove should be true when item was removed" in {
      val map = new SyncConcurrentMap[Int, String]

      map.put(1, "one")
      map.remove(1, "one") shouldEqual true
    }

    "remove should be false when item was not removed due to value difference" in {
      val map = new SyncConcurrentMap[Int, String]

      map.put(1, "one")
      map.remove(1, "two") shouldEqual false
    }

    "remove should be false when item was not removed due to key missing" in {
      val map = new SyncConcurrentMap[Int, String]

      map.remove(1, "one") shouldEqual false
    }

    "remove return a value when removed by existing key" in {
      val map = new SyncConcurrentMap[Int, String]

      map.put(1, "one")
      map.remove(1) shouldEqual Some("one")
    }

    "remove return a None when removed by non-existing key" in {
      val map = new SyncConcurrentMap[Int, String]

      map.remove(1) shouldEqual None
    }

    "replace with value check, should replace when value matches" in {
      val map = new SyncConcurrentMap[Int, String]

      map.put(1, "one")
      map.replace(1, "one", "two") shouldEqual true
      map.get(1) shouldEqual Some("two")
    }

    "replace with value check, should not replace when value does not match" in {
      val map = new SyncConcurrentMap[Int, String]

      map.put(1, "one")
      map.replace(1, "two", "three") shouldEqual false
      map.get(1) shouldEqual Some("one")
    }

    "replace with value check, should not replace when key does not exist" in {
      val map = new SyncConcurrentMap[Int, String]

      map.put(1, "one")
      map.replace(2, "two", "three") shouldEqual false
      map.get(2) shouldEqual None
    }

    "replace should replace when key exist" in {
      val map = new SyncConcurrentMap[Int, String]

      map.put(1, "one")
      map.replace(1, "two") shouldEqual Some("one")
      map.get(1) shouldEqual Some("two")
    }

    "replace should not replace when key does not exist" in {
      val map = new SyncConcurrentMap[Int, String]

      map.replace(1, "one") shouldEqual None
      map.get(1) shouldEqual None
    }

    "+= should add a new pair to map" in {
      val map = new SyncConcurrentMap[Int, String]

      map += 1 -> "one"
      map.get(1) shouldEqual Some("one")
    }

    "-= should remove a key from map" in {
      val map = new SyncConcurrentMap[Int, String]

      map.put(1, "one")
      map -= 1
      map.get(1) shouldEqual None
    }

    "get should return Some(value) when key exists" in {
      val map = new SyncConcurrentMap[Int, String]

      map.put(1, "one")
      map.get(1) shouldEqual Some("one")
    }
    "get should return None when key does not exist" in {
      val map = new SyncConcurrentMap[Int, String]

      map.get(1) shouldEqual None
    }

    "iterator return an iterator of underlying map" in {
      val map = new SyncConcurrentMap[Int, String]

      map.put(1, "one")
      map.put(2, "two")
      map.iterator.toSeq should containTheSameElementsAs(Seq(1 -> "one", 2 -> "two"))
    }
  }

  "ConcurrentSortedList" in {
    "put should add one element correctly" in {
      val list = new ConcurrentSortedList[Int]()
      list.add(5)

      list.iterator.toSeq should containTheSameElementsAs(Seq(5))
    }

    "put should add 3 unordered elements correctly" in {
      val list = new ConcurrentSortedList[Int]()
      list.add(2)
      list.add(3)
      list.add(1)

      list.iterator.toSeq should containTheSameElementsAs(Seq(1, 2, 3))
    }

    "pub should add 20 concurrent elements correctly" in {
      val list = new ConcurrentSortedList[Int]()

      val values = Seq.fill(20)(Random.nextInt)
      values.par.foreach(list.add)

      values.iterator.toSeq should containTheSameElementsAs(values.sorted)
    }

    "iterator should be Nil for empty list" in {
      val list = new ConcurrentSortedList[Int]()
      list.iterator.toSeq should containTheSameElementsAs(Nil)
    }

    "iterator should throw when list has been modified during the iteration" in {
      val list = new ConcurrentSortedList[Int]()
      list.add(1)

      val iterator = list.iterator
      list.add(10)
      iterator.next() should throwA[ConcurrentModificationException]
    }
  }
}
