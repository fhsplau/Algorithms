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
    assert(qc.sort(ArrayBuffer(6, 4, 1)) === ArrayBuffer(1, 4, 6))
  }

  test("should return sorted list if input is sorted") {
    assert(qc.sort(ArrayBuffer(1, 2, 3)) === ArrayBuffer(1, 2, 3))
  }

  test("more than one the same int in the list") {
    assert(qc.sort(ArrayBuffer(6, 4, 1, 6, 2, 1)) === ArrayBuffer(1, 1, 2, 4, 6, 6))
  }

  test("huge array") {
    var r = Random
    val a: Seq[Int] = 1 to 1000000 map { _ => r.nextInt()}
    val array: ArrayBuffer[Int] = ArrayBuffer(a: _*)

    assert(qc.sort(array) === array.sorted)
  }

}
