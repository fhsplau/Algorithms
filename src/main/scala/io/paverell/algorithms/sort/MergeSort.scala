package io.paverell.algorithms.sort

import scala.collection.mutable.ArrayBuffer

class MergeSort(mergeVersion: String = "normal") {
  if (mergeVersion != "normal" && mergeVersion != "recursive")
    throw new Exception(mergeVersion + " does not exist")

  def sort(list: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    def merge(left: ArrayBuffer[Int], right: ArrayBuffer[Int], lm: ArrayBuffer[Int]): ArrayBuffer[Int] = {

      def mergeWhile: ArrayBuffer[Int] = {
        var i, j, k = 0

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
        var lmIndex, leftIndex, rightIndex = 0

        def mergeWith(array: ArrayBuffer[Int], onIndex: Int): ArrayBuffer[Int] = {
          if(lmIndex >= lm.size) lm
          else {
            lm(lmIndex) = array(onIndex)
            lmIndex += 1
            mergeWith(array, onIndex + 1)
          }
        }

        def compareIndexes(): Unit = {
          leftIndex match {
            case i if left(i) <= right(rightIndex) =>
              lm(lmIndex) = left(i)
              leftIndex += 1
            case i if left(i) > right(rightIndex) =>
              lm(lmIndex) = right(rightIndex)
              rightIndex += 1
          }

          lmIndex += 1
        }

        def mergeImpl(): ArrayBuffer[Int] = leftIndex match {
          case i if leftIndex >= left.size => mergeWith(right, rightIndex)
          case i if rightIndex >= right.size => mergeWith(left, leftIndex)
          case _ =>
            compareIndexes()
            mergeImpl()
        }

        mergeImpl()
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
