package io.paverell.algorithms.sort

import scala.collection.mutable.ArrayBuffer

class QuickSort {

  def sort(inputArray: ArrayBuffer[Int]): ArrayBuffer[Int] = {

    val array: ArrayBuffer[Int] = inputArray

    def swap(first: Int, second: Int): Unit = {
      val tmp = array(first)
      array(first) = array(second)
      array(second) = tmp
    }

    def partition(left: Int, right: Int): Int = {
      var rightIndex = right - 1
      var leftIndex = left
      var condition = true

      while (condition) {
        if (leftIndex >= rightIndex) condition = false
        else {
          while (array(rightIndex) > array(right) && rightIndex > leftIndex) rightIndex -= 1
          while (array(leftIndex) <= array(right) && leftIndex < right) leftIndex += 1

          if (array(leftIndex) >= array(right) && array(rightIndex) <= array(right) &&
            leftIndex <= rightIndex)
            swap(leftIndex, rightIndex)
        }
      }

      var newPivotsIndex = leftIndex

      while (array(newPivotsIndex) <= array(right) && newPivotsIndex < right) newPivotsIndex += 1

      swap(newPivotsIndex, right)
      newPivotsIndex
    }

    def impl(left: Int, right: Int): ArrayBuffer[Int] = {
      if (right <= left) array
      else {
        val pivotsIndex = partition(left, right)
        impl(left, pivotsIndex - 1)
        impl(pivotsIndex + 1, right)
      }
    }

    impl(0, array.size - 1)
  }

}
