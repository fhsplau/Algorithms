package io.paverell.algorithms.sort

import scala.collection.mutable.ArrayBuffer

class MergeSort {

  def sort(list: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    val listSize = list.size

    def merge(l: ArrayBuffer[Int], r: ArrayBuffer[Int], listToMerge: ArrayBuffer[Int]): ArrayBuffer[Int] = {
      var i = 0
      var j = 0
      var k = 0

      while(i < l.size && j < r.size) {
        if(l(i) < r(j)){
          listToMerge(k) = l(i)
          i += 1
        } else {
          listToMerge(k) = r(j)
          j += 1
        }
        k += 1
      }

      while(i < l.size) {
        listToMerge(k) = l(i)
        i += 1
        k += 1
      }

      while(j < r.size) {
        listToMerge(k) = r(j)
        j += 1
        k += 1
      }

      listToMerge
    }

    def split(l: ArrayBuffer[Int], r: ArrayBuffer[Int], listToMerge: ArrayBuffer[Int]): ArrayBuffer[Int] = {
      if(l.size < 2 && r.size < 2) merge(l, r, listToMerge)
      else{
        merge(
          split(l take l.size/2, l drop l.size/2, l),
          split(r take r.size/2, r drop r.size/2, r),
          listToMerge
        )
      }

    }

    if(listSize <= 1) list
    else split(list take listSize/2, list drop listSize/2, list)
  }

}
