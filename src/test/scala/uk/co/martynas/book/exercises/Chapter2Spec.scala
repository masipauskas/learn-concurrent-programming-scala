package uk.co.martynas.book.exercises

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent._

import org.specs2.mutable.Specification

class Chapter2Spec extends Specification {
  import Chapter2._
  "parallel" should {
    "execute 2 tasks in parallel" in {
      val latch = new CountDownLatch(2)

      val (a, b) = parallel({
        latch.countDown()
        latch.await(200, TimeUnit.MILLISECONDS)
        2
      },
      {
        latch.countDown()
        latch.await(200, TimeUnit.MILLISECONDS)
        5
      })

      a shouldEqual 2
      b shouldEqual 5
    }
  }

  "periodic" should {
    "execute the task every 200ms" in {
      val counter = new AtomicInteger(0)

      periodically(200) {
        counter.incrementAndGet()
      }

      Thread.sleep(400)
      counter.get() shouldEqual 3
    }
  }

  "SyncVar" should {
    "set and return value if empty" in {
      val variable = new SyncVar[Int]

      variable.set(5)
      variable.get() shouldEqual 5
    }

    "set and set throws exception" in {
      val variable = new SyncVar[Int]

      variable.set(5)
      variable.set(5) should throwAn[IllegalArgumentException]
    }

    "get throws exception if new value" in {
      val variable = new SyncVar[Int]

      variable.get() should throwAn[IllegalArgumentException]
    }

    "set, get, get throws an exception" in {
      val variable = new SyncVar[Int]

      variable.set(5)
      variable.get() shouldEqual 5

      variable.get() should throwAn[IllegalArgumentException]
    }

    "set, get, set, get returns correct values" in {
      val variable = new SyncVar[Int]

      variable.set(5)
      variable.get() shouldEqual 5

      variable.set(2)
      variable.get() shouldEqual 2
    }

    "isEmpty true when has no value" in {
      val variable = new SyncVar[Int]

      variable.isEmpty shouldEqual true
    }

    "nonEmpty false when has no value" in {
      val variable = new SyncVar[Int]

      variable.nonEmpty shouldEqual false
    }

    "isEmpty false when has value" in {
      val variable = new SyncVar[Int]
      variable.set(1)

      variable.isEmpty shouldEqual false
    }

    "nonEmpty true when has value" in {
      val variable = new SyncVar[Int]
      variable.set(1)

      variable.nonEmpty shouldEqual true
    }

    "spinlock methods" should {
      val pool = new ScheduledThreadPoolExecutor(2)
      "setWait sets when variable is initiallyEmpty" in {
        val variable = new SyncVar[Int]
        variable.setWait(5)

        variable.getWait() shouldEqual 5
      }

      "setWait sets with spinlock, when other thread will make variable empty" in {
        val variable = new SyncVar[Int]
        variable.set(5)
        pool.schedule(() => variable.get(), 50, TimeUnit.MILLISECONDS)
        variable.setWait(10)

        variable.get() shouldEqual 10
      }

      "getWait returns when other thread set the variable" in {
        val variable = new SyncVar[Int]
        pool.schedule(new Runnable {
          override def run(): Unit = variable.set(10)
        }, 50, TimeUnit.MILLISECONDS)

        variable.getWait() shouldEqual 10
      }
    }
  }

  "SyncQueue" should {
    "set, get ok" in {
      val variable = new SyncQueue[Int](1)

      variable.set(5)
      variable.get() shouldEqual 5
    }

    "set, set, get, get ok" in {
      val variable = new SyncQueue[Int](2)
      variable.set(5)
      variable.set(10)

      variable.get() shouldEqual 5
      variable.get() shouldEqual 10
    }

    "get exception" in {
      val variable = new SyncQueue[Int](1)

      variable.get() should throwAn[UnsupportedOperationException]
    }

    "set, get, get exception" in {
      val variable = new SyncQueue[Int](2)
      variable.set(5)

      variable.get() shouldEqual 5
      variable.get() should throwAn[UnsupportedOperationException]
    }

    "set, set exception when size 1" in {
      val variable = new SyncQueue[Int](1)
      variable.set(5)

      variable.set(10) should throwAn[IllegalArgumentException]
    }

    "isEmpty true empty" in {
      val variable = new SyncQueue[Int](1)

      variable.isEmpty shouldEqual true
    }

    "nonEmpty false when empty" in {
      val variable = new SyncQueue[Int](1)

      variable.nonEmpty shouldEqual false
    }

    "isEmpty false when has value" in {
      val variable = new SyncQueue[Int](1)
      variable.set(1)

      variable.isEmpty shouldEqual false
    }

    "nonEmpty true when has value" in {
      val variable = new SyncQueue[Int](1)
      variable.set(1)

      variable.nonEmpty shouldEqual true
    }

    "spinlock methods" should {
      val pool = new ScheduledThreadPoolExecutor(2)
      "setWait sets when variable is initiallyEmpty" in {
        val variable = new SyncQueue[Int](1)
        variable.setWait(5)
        variable.getWait() shouldEqual 5
      }

      "setWait sets with spinlock, when other thread will make variable empty" in {
        val variable = new SyncQueue[Int](1)
        variable.set(5)
        pool.schedule(() => variable.get(), 50, TimeUnit.MILLISECONDS)
        variable.setWait(10)

        variable.get() shouldEqual 10
      }

      "getWait returns when other thread set the variable" in {
        val variable = new SyncQueue[Int](1)
        pool.schedule(new Runnable {
          override def run(): Unit = variable.set(10)
        }, 50, TimeUnit.MILLISECONDS)

        variable.getWait() shouldEqual 10
      }
    }
  }

  "Bank" should {
    "sendAll should transfer all money expected" in {
      val source = Set(
        new Account("Test", 100),
        new Account("Test2", 200)
      )

      val target = new Account("Target", 0)
      Bank.sendAll(source, target)

      target.money shouldEqual 300
    }
  }

  "PriorityTaskPool" should {
    "execute lineary based on priority" in {
      val pool = new PriorityTaskPool()
      val concurrentList = new CopyOnWriteArrayList[Int]()
      val latch = new CountDownLatch(3)
      val barrier = new CountDownLatch(1)
      pool.asynchronous(10){
        barrier.await()
        concurrentList.add(10)
        latch.countDown()
      }

      pool.asynchronous(8){
        barrier.await()
        concurrentList.add(8)
        latch.countDown()
      }

      pool.asynchronous(9){
        barrier.await()
        concurrentList.add(9)
        latch.countDown()
      }

      barrier.countDown()
      latch.await()
      pool.shutdown()
      concurrentList.get(0) shouldEqual 10
      concurrentList.get(1) shouldEqual 9
      concurrentList.get(2) shouldEqual 8
    }

    "execute lineary based on priority and ignore low priority items" in {
      val pool = new PriorityTaskPool()
      val concurrentList = new CopyOnWriteArrayList[Int]()
      val latch = new CountDownLatch(2)

      pool.asynchronous(10){
        concurrentList.add(10)
        latch.countDown()
      }

      pool.asynchronous(8){
        concurrentList.add(8)
        latch.countDown()
      }

      pool.asynchronous(3){
        concurrentList.add(9)
        latch.countDown()
      }
      pool.shutdown()

      latch.await()
      concurrentList.get(0) shouldEqual 10
      concurrentList.get(1) shouldEqual 8
    }

    "execute in parallel based on priority" in {
      val pool = new PriorityTaskPool(2)
      val concurrentList = new CopyOnWriteArrayList[Int]()
      val latch = new CountDownLatch(2)
      val firstBlock = new CountDownLatch(1)

      pool.asynchronous(10){
        firstBlock.await()
        concurrentList.add(10)
        latch.countDown()
      }

      pool.asynchronous(8){
        concurrentList.add(8)
        latch.countDown()
      }

      pool.asynchronous(3){
        concurrentList.add(3)
        latch.countDown()
        firstBlock.countDown()
      }

      latch.await()
      pool.shutdown()
      concurrentList.get(0) shouldEqual 8
      concurrentList.get(1) shouldEqual 3
      concurrentList.get(2) shouldEqual 10
    }
  }
}
