package io.paverell.algorithms.sort

import scala.collection.mutable.ArrayBuffer

class MergeSort {

  def sort(list: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    def merge(l: ArrayBuffer[Int], r: ArrayBuffer[Int], listToMerge: ArrayBuffer[Int]): ArrayBuffer[Int] = {
      var i = 0
      var j = 0
      var k = 0

      def updateIndexes(ix: Int, jx: Int, kx: Int): Unit = {
        i += ix
        j += jx
        k += kx
      }

      while (i < l.size && j < r.size) {
        if (l(i) < r(j)) {
          listToMerge(k) = l(i)
          updateIndexes(1, 0, 0)
        } else {
          listToMerge(k) = r(j)
          updateIndexes(0, 1, 0)
        }
        updateIndexes(0, 0, 1)
      }

      while (i < l.size) {
        listToMerge(k) = l(i)
        updateIndexes(1, 0, 1)
      }

      while (j < r.size) {
        listToMerge(k) = r(j)
        updateIndexes(0, 1, 1)
      }

      listToMerge
    }

    def sortImpl(l: ArrayBuffer[Int], r: ArrayBuffer[Int], listToMerge: ArrayBuffer[Int]): ArrayBuffer[Int] = {
      if (l.size < 2 && r.size < 2) merge(l, r, listToMerge)
      else {
        merge(
          sortImpl(l take l.size / 2, l drop l.size / 2, l),
          sortImpl(r take r.size / 2, r drop r.size / 2, r),
          listToMerge
        )
      }

    }

    sortImpl(list take list.size / 2, list drop list.size / 2, list)
  }

}
