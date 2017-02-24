package com.nthportal.testing.concurrent

import java.util.concurrent.{ConcurrentLinkedQueue, Executor}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

/**
  * An [[Executor]] whose tasks can be executed manually in
  * the current thread when desired, for fine-grained control
  * over when tasks are executed.
  */
final class ManualExecutor extends Executor {
  private val queue = new ConcurrentLinkedQueue[Runnable]()

  override def execute(runnable: Runnable) = queue.add(runnable)

  /**
    * Returns the number of tasks waiting to be executed.
    *
    * @return the number of tasks waiting to be executed
    */
  def waitingTasks: Int = queue.size()

  /**
    * Executes all tasks waiting to be executed.
    *
    * @return the number of tasks executed
    */
  def executeAll(): Int = executeAllImpl(0)

  @tailrec
  private def executeAllImpl(executed: Int): Int = {
    val r = queue.poll()
    if (r == null) executed
    else {
      r.run()
      executeAllImpl(executed + 1)
    }
  }

  /**
    * Executes `count` tasks waiting to be executed, or all
    * tasks if there are fewer waiting.
    *
    * @param count the maximum number of tasks to execute
    */
  def executeAtMost(count: Int): Int = executeAtMostImpl(count, 0)

  @tailrec
  private def executeAtMostImpl(count: Int, executed: Int): Int = {
    if (count <= 0) executed
    else {
      val r = queue.poll()
      if (r == null) executed
      else {
        r.run()
        executeAtMostImpl(count - 1, executed + 1)
      }
    }
  }

  /**
    * Executes the next waiting task, if one exists.
    *
    * @return `true` if the next tasks was executed; `false`
    *         if there are no tasks waiting
    */
  def tryExecuteNext(): Boolean = {
    val r = queue.poll()
    if (r == null) false
    else {
      r.run()
      true
    }
  }

  /**
    * Executes the next waiting task.
    *
    * @throws NoSuchElementException if there is no task waiting
    *                                to be executed
    */
  @throws[NoSuchElementException]
  def executeNext(): Unit = if (!tryExecuteNext()) throw new NoSuchElementException("No tasks to execute")

  /**
    * Contains this [[Executor]] as an `implicit` [[ExecutionContext]].
    */
  object Implicits {
    implicit val asExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(ManualExecutor.this)
  }
}

object ManualExecutor {
  /**
    * Returns a new [[ManualExecutor]].
    *
    * @return a new ManualExecutor
    */
  def apply(): ManualExecutor = new ManualExecutor
}
