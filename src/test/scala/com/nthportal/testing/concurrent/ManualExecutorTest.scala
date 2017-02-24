package com.nthportal.testing.concurrent

import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class ManualExecutorTest extends FlatSpec with Matchers {

  import ManualExecutorTest._

  behavior of "ManualExecutorTest"

  it should "execute all waiting tasks" in {
    val executor = ManualExecutor()
    val executables = List.fill(3)(new Executable)

    executables.foreach(executor.execute(_))
    executor.waitingTasks should be (3)

    executor.executeAll() should be (3)
    executor.waitingTasks should be (0)
    executables.forall(_.executed) should be (true)
  }

  it should "execute at most some number of waiting tasks" in {
    val executor = ManualExecutor()
    val executables = List.fill(3)(new Executable)

    executables.foreach(executor.execute(_))
    executor.waitingTasks should be (3)

    executor.executeAtMost(2) should be (2)
    executor.waitingTasks should be (1)
    executables.head.executed should be (true)
    executables(1).executed should be (true)

    executor.executeAtMost(2) should be (1)
    executor.waitingTasks should be (0)
    executables.forall(_.executed) should be (true)

    executor.executeAtMost(2) should be (0)
    executor.waitingTasks should be (0)
  }

  it should "execute the next waiting task" in {
    val executor = ManualExecutor()
    val executables = List.fill(3)(new Executable)

    executables.foreach(executor.execute(_))
    executor.waitingTasks should be (3)

    executor.executeNext()
    executor.waitingTasks should be (2)
    executables.head.executed should be (true)

    executor.executeNext()
    executor.waitingTasks should be (1)
    executables(1).executed should be (true)

    executor.executeNext()
    executor.waitingTasks should be (0)
    executables.forall(_.executed) should be (true)

    a [NoSuchElementException] should be thrownBy {executor.executeNext()}
  }

  it should "behave properly as an `ExecutionContext`" in {
    val executor = ManualExecutor()

    import executor.Implicits._

    val ex = new Executable
    val f = Future {ex.run(); ex}

    ex.executed should be (false)
    f.isCompleted should be (false)

    executor.executeAll()
    ex.executed should be (true)
    f.isCompleted should be (true)
    Await.result(f, Duration.Zero) should be theSameInstanceAs ex
  }

}

object ManualExecutorTest {
  class Executable extends Runnable {
    private var _executed: Boolean = false

    override def run() = _executed = true

    def executed: Boolean = _executed
  }
}
