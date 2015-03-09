package io.paverell.algorithms.sort

import scala.collection.mutable.ArrayBuffer

class QuickSort {

  def sort(array: ArrayBuffer[Int]): ArrayBuffer[Int] = {

    def swap(first: Int, second: Int): Int = {
      val tmp = array(first)
      array(first) = array(second)
      array(second) = tmp
      first
    }

    def partition(left: Int, right: Int): Int = {
      val pivot = right
      var rightIndex = right - 1
      var leftIndex = left

      while (leftIndex < rightIndex) {
        while (array(rightIndex) > array(pivot) && rightIndex > leftIndex) rightIndex -= 1
        while (array(leftIndex) <= array(pivot) && leftIndex < rightIndex) leftIndex += 1
        swap(leftIndex, rightIndex)
      }

      while (array(leftIndex) <= array(pivot) && leftIndex < pivot) leftIndex += 1

      swap(leftIndex, right)
    }

    def impl(left: Int, right: Int): ArrayBuffer[Int] =
      if (right <= left) array
      else {
        val pivotsIndex = partition(left, right)
        impl(left, pivotsIndex - 1)
        impl(pivotsIndex + 1, right)
      }

    impl(0, array.size - 1)
  }

}
