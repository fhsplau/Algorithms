package io.paverell.algorithms.sort

import scala.collection.mutable.ArrayBuffer

class MergeSort(mergeVersion: String = "normal") {
  if (mergeVersion != "normal" && mergeVersion != "recursive")
    throw new Exception(mergeVersion + " does not exist")

  def sort(list: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    def merge(l: ArrayBuffer[Int], r: ArrayBuffer[Int], lm: ArrayBuffer[Int]): ArrayBuffer[Int] = {

      def mergeWhile: ArrayBuffer[Int] = {
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

      def mergeRec: ArrayBuffer[Int] = {
        def mergeWith(toMerge: ArrayBuffer[Int], startIndex: Int, lmIndex: Int): Unit = {
          var tmpLmIndex = lmIndex
          var toMergeIndex = startIndex

          while (tmpLmIndex < lm.size) {
            lm(tmpLmIndex) = toMerge(toMergeIndex)
            tmpLmIndex += 1
            toMergeIndex += 1
          }
        }

        def mergeImpl(nextLeft: Int, nextRight: Int, next: Int): ArrayBuffer[Int] = {
          if (nextRight >= r.size || nextLeft >= l.size) {
            if (nextLeft >= l.size) mergeWith(r, nextRight, next) else mergeWith(l, nextLeft, next)
            lm
          }
          else mergeImpl(
            if (l(nextLeft) <= r(nextRight)) {
              lm(next) = l(nextLeft)
              nextLeft + 1
            } else nextLeft,
            if (r(nextRight) < l(nextLeft)) {
              lm(next) = r(nextRight)
              nextRight + 1
            } else nextRight,
            next + 1
          )
        }

        mergeImpl(0, 0, 0)
      }

      mergeVersion match {
        case "normal" => mergeWhile
        case "recursive" => mergeRec
      }
    }

    def sortImpl(l: ArrayBuffer[Int], r: ArrayBuffer[Int], listToMerge: ArrayBuffer[Int]): ArrayBuffer[Int] =
      if (listToMerge.size <= 2) merge(l, r, listToMerge)
      else {
        val leftSub = sortImpl(l take l.size / 2, l drop l.size / 2, l)
        val rightSub = sortImpl(r take r.size / 2, r drop r.size / 2, r)
        merge(leftSub, rightSub, listToMerge)
      }

    sortImpl(list take list.size / 2, list drop list.size / 2, list)
  }

}
