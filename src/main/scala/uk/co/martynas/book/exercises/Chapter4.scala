package uk.co.martynas.book.exercises

import java.util.concurrent.TimeUnit

import scala.async.Async.{async, await}
import scala.concurrent.duration.Duration
import scala.concurrent._
import scala.util.{Success, Try}

object Chapter4 {

  class IVar[T] {
    private val value: Promise[T] = Promise()
    private val result: Future[T] = value.future

    def apply(): T = Await.result(result, Duration.fromNanos(1))

    def :=(x: T): Unit = value.success(x)
  }

  implicit class FutureOps[T](val future: Future[T]) {
    def exists(predicate: T => Boolean)(implicit ec: ExecutionContext): Future[Boolean] = {
      future
        .map(predicate)
        .recover { case _ => false }
    }

    def existsPromise(predicate: T => Boolean)(implicit ec: ExecutionContext): Future[Boolean] = {
      val promise = Promise[Boolean]()
      future.onComplete {
        case Success(value) => promise.complete(Success(predicate(value)))
        case _ => promise.complete(Success(false))
      }

      promise.future
    }

    def existsAsync(predicate: T => Boolean)(implicit ec: ExecutionContext): Future[Boolean] = {
      async {
        val value = await(future)
        predicate(value)
      }
    }
  }

  class IMap[K, V] {
    private val futures = collection.concurrent.TrieMap[K, Promise[V]]()

    def update(k: K, v: V): Unit = {
      futures.putIfAbsent(k, Promise())
      futures(k).complete(Success(v))
    }

    def apply(k: K): Future[V] = {
      futures.putIfAbsent(k, Promise())
      futures(k).future
    }
  }

  implicit class PromiseOps[T](val promise: Promise[T]) {
    def compose[S](f: S => T)(implicit ec: ExecutionContext): Promise[S] = {
      val p = Promise[S]()
      p.future
        .map(r => f(r))
        .onComplete(v => promise.complete(v))
      p
    }
  }

  def spawn(command: String)(implicit ec: ExecutionContext): Future[Int] = {
    val builder = new ProcessBuilder()
    builder.command(command)

    Future {
      sys.process.Process(command)!
    }
  }

  def execute[T](updateAction: () => Unit, every: Duration, timeout: Duration)(action: () => T)(implicit executionContext: ExecutionContext): T = {
    def update(f: Future[T], fu: () => Unit, every: Duration, remaining: Duration): Future[Unit] = {
      if (f.isCompleted || remaining.toMillis <= 0) Future.fromTry(Success(Unit))
      else {
        Future {
          blocking {
            Try(Await.result(Promise().future, every))
            fu()
            async {
              val result = update(f, fu, every, remaining.minus(every))
              await(result)
            }
          }
        }
      }
    }

    val promise = Promise[T]()
    update(promise.future, updateAction, every, timeout)

    Future {
      blocking {
        val result = Try(action())
        promise.complete(result)
      }
    }

    Await.result(promise.future, timeout)
  }
}
