package io.paverell.algorithms.sort

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.junit.JUnitRunner

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

@RunWith(classOf[JUnitRunner])
class QuicSortTest extends FunSuite with BeforeAndAfter {
  var qc: QuickSort = _

  before {
    qc = new QuickSort
  }

  test("if list is empty sort returns empty list") {
    assert(qc.sort(ArrayBuffer()) === ArrayBuffer())
  }

  test("sort non empty list") {
    assert(qc.sort(ArrayBuffer(1)) === ArrayBuffer(1))
    assert(qc.sort(ArrayBuffer(2,1)) === ArrayBuffer(1,2))
    assert(qc.sort(ArrayBuffer(6, 4, 1)) === ArrayBuffer(1, 4, 6))
  }

  test("should return sorted list if input is sorted") {
    assert(qc.sort(ArrayBuffer(1, 2, 3)) === ArrayBuffer(1, 2, 3))
  }

  test("duplicates in the list") {
    assert(qc.sort(ArrayBuffer(6, 4, 1, 6, 2, 1)) === ArrayBuffer(1, 1, 2, 4, 6, 6))
  }

  test("huge array") {
    val n = 3000000
    val a: Seq[Int] = 1 to n map { _ => Random.nextInt(n-1)}
    val array: ArrayBuffer[Int] = ArrayBuffer(a: _*)

    assert(qc.sort(array) === array.sorted)
  }

}
