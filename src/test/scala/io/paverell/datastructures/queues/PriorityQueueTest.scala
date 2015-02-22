package io.paverell.datastructures.queues

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.junit.JUnitRunner

import PQTestFramework.tf

@RunWith(classOf[JUnitRunner])
class PriorityQueueTest extends FunSuite with BeforeAndAfter {

  var queue: PriorityQueue = _

  before {
    queue = new PriorityQueue(10)
  }

  test("empty queue") {
    assert(queue.isEmpty)
  }

  test("pop from empty queue") {
    intercept[RuntimeException] {
      queue.pop()
    }
  }

  test("after push queue is not empty") {
    queue.push(10,1)
    assert(!queue.isEmpty)
  }

  test("can push element to the queue") {
    queue.push(10,1)

    assert(queue.contains(10))
  }

  test("can push two elements to the queue") {
    queue.multiPush(List((10,1),(12,2),(9,1),(14,1),(15,1),(11,1),(8,1)))

    assert(queue.multiContain(List(14,12,9,8,15,11,10)))

    assert(!queue.contains(1))
  }

  test("can pop from non empty queue") {
    queue = queue multiPush List((10, 1), (12, 2))

    assert(queue.pop() == 2)
  }

  test("pop root element") {
    queue = queue multiPush List((10, 1), (9, 1), (8, 1))
    queue.pop()

    assert(!queue.contains(10))
    assert(queue.multiContain(List(9, 8)))
  }

  test("runtime exception after one push one and two pops") {
    queue.push(10, 1)
    queue.pop()

    intercept[RuntimeException] {
      queue.pop()
    }
  }

  test("after pop queue does not contain element") {
    queue = queue multiPush List((10, 1), (12, 2), (9,1), (8,1), (11,1))
    queue.pop()

    assert(!queue.contains(12))

    assert(queue.multiContain(List(10,9,8,11)))
  }

  test("queue is empty after poped all elements") {
    queue = queue.multiPush(List((10, 1), (12, 2)))
    queue = queue.multiPop(2)

    assert(queue.isEmpty)
  }

  test("size of empty queue equals to 0") {
    assert(queue.size == 0)
  }

  test("after one push size equals to 1") {
    queue.push(10, 1)
    assert(queue.size == 1)
  }

  test("after two pushes and two pops size is equal to 0") {
    queue = queue multiPush List((10, 1), (8, 1))
    queue = queue multiPop 2

    assert(queue.size == 0)
  }

  test("exception if max size exceed") {
    var aQueue = new PriorityQueue(2)
    aQueue = aQueue multiPush List((10, 1), (11, 2))

    intercept[IndexOutOfBoundsException] {
      aQueue.push(1, 1)
    }
  }

  test("no exception about max size after pop") {
    var aQueue = new PriorityQueue(2)
    aQueue = aQueue multiPush List((10, 1), (11, 2))

    intercept[IndexOutOfBoundsException] {
      aQueue.push(1, 1)
    }

    aQueue.pop()
    aQueue.push(20, 1)
    assert(aQueue.contains(20))
  }

  test("highest priority in empty queue is equal to null") {
    assert(queue.highestPriority == null)
  }

  test("highest priority") {

    var hp = 0

    def pushAndAssertHP(p: Int, t: Int): Unit = {
      hp = if (p > hp) p else hp
      queue.push(p, t)
      assert(queue.highestPriority == hp)
    }

    val toPush = List((10, 1), (12, 2), (9, 1), (11, 0))
    for (t <- toPush) {
      pushAndAssertHP(t._1, t._2)
    }
  }

  test("after queue is poped to empty hp is equal to null") {
    queue = queue multiPush List((10, 1), (1, 1))
    queue = queue multiPop 2

    assert(queue.highestPriority == null)
  }

}