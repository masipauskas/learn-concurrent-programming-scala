package uk.co.martynas.book.exercises

import java.util.UUID
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}

import uk.co.martynas.book.exercises.Chapter2.SyncVar.{Empty, Holder, NonEmpty}

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object Chapter2 {
  private val scheduledExecutor: ScheduledThreadPoolExecutor = new ScheduledThreadPoolExecutor(1)
  def parallel[A, B](a : => A, b : => B): (A, B) = {
    val fA = Future(a)
    val fB = Future(b)

    val result = for {
      rA <- fA
      rB <- fB
    } yield (rA, rB)

    Await.result(result, Duration.Inf)
  }

  def periodically(duration: Long)(b : => Unit): Unit = {
    scheduledExecutor.scheduleAtFixedRate(() => b, 0, 200, TimeUnit.MILLISECONDS)
  }

  trait Sync[T] {
    def get(): T

    def getWait(): T = {
      while(isEmpty) { }
      get()
    }

    def set(value: T)

    def setWait(value: T): Unit = {
      while(nonEmpty) { }
      set(value)
    }

    def isEmpty: Boolean
    def nonEmpty: Boolean = !isEmpty
  }

  class SyncQueue[T](val max: Int) extends Sync[T] {
    private val obj = new AtomicReference[List[T]](Nil)

    def get(): T = {
      val result = obj.get()
      if (obj.compareAndSet(result, result.init)) {
        result.last
      } else get()
    }

    def set(value: T): Unit = {
      val current = obj.get()
      if (current.length < max) {
        if (!obj.compareAndSet(current, value :: current)) {
          set(value)
        }
      } else throw new IllegalArgumentException("Queue is full")
    }

    override def setWait(value: T): Unit = {
      while(isFull) { }
      set(value)
    }

    def isEmpty: Boolean = obj.get() == Nil
    def isFull: Boolean = obj.get().length == max
  }

  class SyncVar[T] extends Sync[T] {
    private val empty = new Empty[T]()
    private val obj = new AtomicReference[Holder[T]](empty)

    def get(): T = {
      val result = obj.getAndSet(empty)
      result.value
    }

    def set(value: T): Unit = {
      val success = obj.compareAndSet(empty, NonEmpty(value))
      if (!success) throw new IllegalArgumentException("Set from non-empty SyncVar")
    }

    def isEmpty: Boolean = obj.get() == empty
  }

  object SyncVar {
    trait Holder[T] {
      def value: T
    }

    case class NonEmpty[T](value: T) extends Holder[T]
    case class Empty[T]() extends Holder[T] {
      override def value: T = throw new IllegalArgumentException("Get from Empty")
    }
  }


  class Account(val name: String, var money: Int) {
    val uid = UUID.randomUUID().toString
  }

  object Bank {
    private def send(a1: Account, a2: Account) {
      def adjust() {
        a2.money += a1.money
        a1.money -= a1.money
      }
      if (a1.uid < a2.uid)
        a1.synchronized { a2.synchronized { adjust() } }
      else a2.synchronized { a1.synchronized { adjust() } }
    }

    def sendAll(accounts: Set[Account], target: Account): Unit = {
      accounts.par.foreach(account => send(account, target))
    }
  }

  class PriorityTaskPool(val workers: Int = 1) {
    private val tasks = mutable.PriorityQueue[Task]()
    private val threads = (0 until workers).map(_ => new Worker(tasks))

    def asynchronous(priority: Int)(task: => Unit): Unit = {
      tasks.synchronized {
        tasks.enqueue(new Task(priority, () => task))
        tasks.notify()
      }
    }

    def shutdown(): Unit = threads.foreach(_.shutdown = true)

    private class Worker(val pool: mutable.PriorityQueue[Task]) extends Thread {
      private val empty = new Task(-1, () => Unit)
      private val important = 5
      @volatile var shutdown: Boolean = false
      this.start()

      private def consume(): Unit = {
        var task: Task = empty
        pool.synchronized {
          if (pool.nonEmpty) {
            task = pool.dequeue()
          } else pool.wait(500)
        }

        if (!(shutdown && task.priority < important)) {
          if (task != empty) {
            task.execute
          }
          consume()
        }
      }

      override def run(): Unit = consume()
    }
  }

  class Task(val priority: Int, task: () => Unit) extends Ordered[Task] {
    override def compare(that: Task): Int = priority - that.priority
    def execute: Unit = task()
  }
}