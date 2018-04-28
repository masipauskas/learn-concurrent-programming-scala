package uk.co.martynas.book.exercises

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import scala.annotation.tailrec
import scala.compat.Platform.ConcurrentModificationException
import scala.concurrent.ExecutionContext
import scala.util.Try

object Chapter3 {
  object PiggyBackContext extends ExecutionContext {
    override def execute(runnable: Runnable): Unit = {
      Try(runnable.run()).fold(error => reportFailure(error), _ => Unit)
    }

    override def reportFailure(cause: Throwable): Unit = throw cause
  }

  class TreiberStack[T] {
    private val holder = new AtomicReference[List[T]](Nil)
    def push(x: T): Unit = {
      val value = holder.get()
      if (!holder.compareAndSet(value, x :: value)) push(x)
    }
    def pop(): T = {
      val value = holder.get()
      if (holder.compareAndSet(value, value.tail)) {
        value.head
      } else pop()
    }
  }

  //todo: Implement me
  sealed class ConcurrentSortedList[T](implicit val ord: Ordering[T]) {
    private val isInserting = new AtomicBoolean(false)
    private var list = collection.immutable.List.empty[T]
    @tailrec
    final def add(x: T): Unit = {
      if (isInserting.compareAndSet(false, true)) {
        val (left, right) = list.span(ord.lt(_, x))
        list = left ++ (x :: right)
        isInserting.set(false)
      } else add(x)
    }

    final def iterator: Iterator[T] = new Iterator[T] {
      private val underlyingList = list
      private val underlying = list.iterator
      override def hasNext: Boolean = {
        if (underlyingList == list) underlying.hasNext
        else throw new ConcurrentModificationException
      }

      override def next(): T = {
        if (underlyingList == list) underlying.next()
        else throw new ConcurrentModificationException
      }
    }
  }

  class LazyCell[T](initialization: => T) {
    private val inProgress = new AtomicBoolean(false)
    private val result = new AtomicReference[Option[T]](None)
    def apply(): T = {
      if (result.get().isEmpty) {
        if (inProgress.compareAndSet(false, true)) {
          result.set(Some(initialization))
        } else apply()
      }
      result.get.get
    }
  }

  class PureLazyCell[T](initialization: => T) {
    private val result = new AtomicReference[Option[T]](None)
    def apply(): T = {
      if (result.get().isEmpty) result.compareAndSet(None, Some(initialization))
      result.get.get
    }
  }

  class SyncConcurrentMap[A, B] extends collection.concurrent.Map[A, B] {
    private val underlying = collection.mutable.Map[A, B]()
    override def putIfAbsent(k: A, v: B): Option[B] = {
      underlying.synchronized {
        val result: Option[B] = underlying.get(k)
        if (result.isEmpty) {
          underlying.put(k, v)
        }
        result
      }
    }

    override def remove(k: A, v: B): Boolean = {
      underlying.synchronized {
        if (underlying.get(k).contains(v)) {
          underlying.remove(k).nonEmpty
        } else false
      }
    }

    override def replace(k: A, oldvalue: B, newvalue: B): Boolean = {
      underlying.synchronized {
        if (underlying.get(k).contains(oldvalue)) {
          underlying.put(k, newvalue)
          true
        } else false
      }
    }

    override def replace(k: A, v: B): Option[B] = {
      underlying.synchronized {
        val current = underlying.get(k)
        if (current.nonEmpty) {
          underlying.put(k, v)
        }

        current
      }
    }

    override def +=(kv: (A, B)): SyncConcurrentMap.this.type = {
      underlying.synchronized {
        underlying.put(kv._1, kv._2)
        this
      }
    }

    override def -=(key: A): SyncConcurrentMap.this.type = {
      underlying.synchronized {
        underlying.remove(key)
        this
      }
    }


    override def get(key: A): Option[B] = {
      underlying.synchronized {
        underlying.get(key)
      }
    }

    override def iterator: Iterator[(A, B)] = {
      underlying.synchronized {
        underlying.iterator
      }
    }
  }
}
