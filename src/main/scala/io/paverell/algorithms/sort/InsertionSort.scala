package io.paverell.algorithms.sort

import scala.collection.mutable.ListBuffer

class InsertionSort {

  def sort(l: List[Int]): List[Int] = {

    val tmpL: ListBuffer[Int] = ListBuffer(l: _*)

    for (x <- 0 to tmpL.size - 1) {
      for (y <- 1 to tmpL.size - 1) {
        if (tmpL(y - 1) > tmpL(y)) {
          val tmp = tmpL(y - 1)
          tmpL(y - 1) = tmpL(y)
          tmpL(y) = tmp
        }
      }
    }

    List(tmpL: _*)
  }

}
