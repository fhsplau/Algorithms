package io.paverell.datastructures.queues

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.junit.JUnitRunner

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
      queue.pop
    }
  }

  test("can pop from non empty queue") {
    queue = multiPush(queue, List((10, 1), (12, 2)))

    assert(queue.pop == 2)
  }

  test("pop root element") {
    queue = multiPush(queue, List((10, 1), (9, 1), (8, 1)))
    queue.pop

    assert(!queue.contains(10))
    assert(multiContain(queue, List(9, 8)))
  }

  test("runtime exception after one push one and two pops") {
    queue.push(10, 1)
    queue.pop

    intercept[RuntimeException] {
      queue.pop
    }
  }

  test("after pop queue does not contain element") {
    queue = multiPush(queue, List((10, 1), (12, 2)))
    queue.pop

    assert(!queue.contains(12))
    assert(queue.contains(10))
  }

  test("queue is empty after poped all elements") {
    queue = multiPush(queue, List((10, 1), (12, 2)))
    queue = multiPop(queue, 2)

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
    queue = multiPush(queue, List((10, 1), (8, 1)))
    queue = multiPop(queue, 2)

    assert(queue.size == 0)
  }

  test("exception if max size excided") {
    var aQueue = new PriorityQueue(2)
    aQueue = multiPush(aQueue, List((10, 1), (11, 2)))

    intercept[IndexOutOfBoundsException] {
      aQueue.push(1, 1)
    }
  }

  test("no excepion about max size after pop") {
    var aQueue = new PriorityQueue(2)
    aQueue = multiPush(aQueue, List((10, 1), (11, 2)))

    intercept[IndexOutOfBoundsException] {
      aQueue.push(1, 1)
    }

    aQueue.pop
    aQueue.push(20, 1)
    assert(aQueue.contains(20))
  }

  test("highest priority in empty queue is 0") {
    assert(queue.highestPriority == null)
  }

  test("can add to queue") {
    queue = multiPush(queue, List((10, 100), (12, 0), (8, 0)))
    multiContain(queue, List(10, 12, 8))
  }

  test("highest priority") {

    var hp = 0

    def pushAndAssertHP(p: Int, t: Int): Unit = {
      hp = if (p > hp) p else hp
      queue.push(p, t)
      assert(queue.highestPriority.get == hp)
    }

    val toPush = List((10, 1), (12, 2), (9, 1), (11, 0))
    for (t <- toPush) {
      pushAndAssertHP(t._1, t._2)
    }
  }

  test("after queue is poped to empty hp is equal to 0") {
    queue = multiPush(queue, List((10, 1), (1, 1)))
    queue = multiPop(queue, 2)

    assert(queue.highestPriority == null)
  }

  def multiPush(q: PriorityQueue, toPush: List[(Int, Int)]): PriorityQueue = {
    for (push <- toPush) {
      q.push(push._1, push._2)
    }

    q
  }

  def multiPop(q: PriorityQueue, howMany: Int): PriorityQueue = {
    for (t <- 0 to howMany - 1) {
      q.pop
    }

    q
  }

  def multiContain(q: PriorityQueue, piorityList: List[Int]): Boolean =
    if (piorityList.isEmpty) true
    else q.contains(piorityList.head) && multiContain(q, piorityList.tail)
}
