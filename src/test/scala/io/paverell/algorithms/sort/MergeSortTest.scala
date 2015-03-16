package io.paverell.algorithms.sort

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.junit.JUnitRunner
import scala.util.Random

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class MergeSortTest extends FunSuite with BeforeAndAfter{
  var ms: MergeSort = _

  before {
    ms = new MergeSort
  }

  test("if list is empty sort returns empty list") {
    assert(ms.sort(ArrayBuffer()) === ArrayBuffer())
  }

  test("sort non empty list") {
    assert(ms.sort(ArrayBuffer(1)) === ArrayBuffer(1))
    assert(ms.sort(ArrayBuffer(2,1)) === ArrayBuffer(1,2))
    assert(ms.sort(ArrayBuffer(6, 4, 1)) === ArrayBuffer(1, 4, 6))
  }

  test("should return sorted list if input is sorted") {
    assert(ms.sort(ArrayBuffer(1, 2, 3)) === ArrayBuffer(1, 2, 3))
  }

  test("duplicates in the list") {
    assert(ms.sort(ArrayBuffer(6, 4, 1, 6, 2, 1)) === ArrayBuffer(1, 1, 2, 4, 6, 6))
  }

  test("huge array") {
    val n = 1000000
    val a: Seq[Int] = 1 to n map { _ => Random.nextInt(n-1)}
    val array: ArrayBuffer[Int] = ArrayBuffer(a: _*)
    val arraySorted = array.sorted
    assert(array!=arraySorted)

    assert(ms.sort(array) === arraySorted)
  }

  test("recursive algorithm") {
    val rec = new MergeSort("recursive")
    val n = 1000000
    val a: Seq[Int] = 1 to n map { _ => Random.nextInt(n-1)}
    val array: ArrayBuffer[Int] = ArrayBuffer(a: _*)
    val arraySorted = array.sorted
    assert(array!=arraySorted)

    assert(rec.sort(array) === arraySorted)
  }

  test("huge array sorted") {
    val n = 1000000
    val a: Seq[Int] = 1 to n
    val array: ArrayBuffer[Int] = ArrayBuffer(a: _*)

    assert(ms.sort(array) === array.sorted)
  }

  test("exception if bad algrorithm name") {
    intercept[Exception] {
      new MergeSort("badAlgorithm")
    }
  }

}
