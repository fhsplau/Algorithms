package io.paverell.algorithms.sort

class MergeSort {

  def sort(list: List[Int]): List[Int] = {
    val listSize = list.size

    def merge(l: List[Int], r: List[Int]): List[Int] = {
      var i = 0
      var j = 0
      var merged: List[Int] = List()

      while(i < l.size && j < r.size) {
        if(l(i) < r(j)){
          merged = merged ::: List(l(i))
          i += 1
        } else {
          merged = merged ::: List(r(j))
          j += 1
        }
      }

      while(i < l.size) {
        merged = merged ::: List(l(i))
        i += 1
      }

      while(j < r.size) {
        merged = merged ::: List(r(j))
        j += 1
      }

      merged
    }

    def split(l: List[Int], r: List[Int]): List[Int] = {
      if(l.size < 2 && r.size < 2) merge(l, r)
      else{
        merge(split(l take l.size/2, l drop l.size/2), split(r take r.size/2, r drop r.size/2))
      }

    }

    if(listSize <= 1) list
    else split(list take listSize/2, list drop listSize/2)
  }

}
