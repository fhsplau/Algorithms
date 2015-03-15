package io.paverell.algorithms.sort

import scala.collection.mutable.ArrayBuffer

class MergeSort(mergeVersion: String = "normal") {
  if (mergeVersion != "normal" && mergeVersion != "recursive")
    throw new Exception(mergeVersion + " does not exist")

  def sort(list: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    def merge(left: ArrayBuffer[Int], right: ArrayBuffer[Int], lm: ArrayBuffer[Int]): ArrayBuffer[Int] = {

      def mergeWhile: ArrayBuffer[Int] = {
        var i = 0
        var j = 0
        var k = 0

        while (i < left.size && j < right.size) {
          if (left(i) < right(j)) {
            lm(k) = left(i)
            i += 1
          } else {
            lm(k) = right(j)
            j += 1
          }
          k += 1
        }

        while (i < left.size) {
          lm(k) = left(i)
          i += 1
          k += 1
        }

        while (j < right.size) {
          lm(k) = right(j)
          j += 1
          k += 1
        }

        lm
      }

      def mergeRec: ArrayBuffer[Int] = {
        var lmIndex = 0

        def mergeWith(toMerge: ArrayBuffer[Int], startIndex: Int): Unit = {
          var toMergeIndex = startIndex

          while (lmIndex < lm.size) {
            lm(lmIndex) = toMerge(toMergeIndex)
            lmIndex += 1
            toMergeIndex += 1
          }
        }

        def mergeImpl(nextLeft: Int, nextRight: Int): Unit = {
          if (nextRight >= right.size || nextLeft >= left.size)
            if (nextLeft >= left.size) mergeWith(right, nextRight) else mergeWith(left, nextLeft)
          else mergeImpl(
            if (left(nextLeft) <= right(nextRight)) {
              lm(lmIndex) = left(nextLeft)
              lmIndex += 1
              nextLeft + 1
            } else nextLeft,
            if (right(nextRight) < left(nextLeft)) {
              lm(lmIndex) = right(nextRight)
              lmIndex += 1
              nextRight + 1
            } else nextRight

          )
        }

        mergeImpl(0, 0)
        lm
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
