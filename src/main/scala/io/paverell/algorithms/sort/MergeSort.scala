package io.paverell.algorithms.sort

import scala.collection.mutable.ArrayBuffer

class MergeSort {

  def sort(list: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    def merge(l: ArrayBuffer[Int], r: ArrayBuffer[Int], lm: ArrayBuffer[Int]): ArrayBuffer[Int] = {
      var i = 0
      var j = 0
      var k = 0

      while (i < l.size && j < r.size) {
        if (l(i) < r(j)) {
          lm(k) = l(i)
          i += 1
        } else {
          lm(k) = r(j)
          j += 1
        }
        k += 1
      }

      while (i < l.size) {
        lm(k) = l(i)
        i += 1
        k += 1
      }

      while (j < r.size) {
        lm(k) = r(j)
        j += 1
        k += 1
      }

      lm
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
