package io.paverell.datastructures.queues

class TestFramework(q: PriorityQueue){

  def multiPush(toPush: List[(Int, Int)]): PriorityQueue = {
    for (push <- toPush) {
      q.push(push._1, push._2)
    }

    q
  }

  def multiPop(howMany: Int): PriorityQueue = {
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
  implicit def tf(q: PriorityQueue): TestFramework = new TestFramework(q)
}