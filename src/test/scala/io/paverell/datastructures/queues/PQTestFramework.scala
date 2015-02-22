package io.paverell.datastructures.queues

class TestFramework[T](q: PriorityQueue[T]){

  def multiPush(toPush: List[(Int, T)]): PriorityQueue[T] = {
    for (push <- toPush) {
      q.push(push._1, push._2)
    }

    q
  }

  def multiPop(howMany: Int): PriorityQueue[T] = {
    for (t <- 0 to howMany - 1) {
      q.pop()
    }

    q
  }

  def multiContain(priorityList: List[Int]): Boolean =
    if (priorityList.isEmpty) true
    else q.contains(priorityList.head) && multiContain(priorityList.tail)
}

object PQTestFramework {
  implicit def tf[T](q: PriorityQueue[T]): TestFramework[T] = new TestFramework[T](q)
}