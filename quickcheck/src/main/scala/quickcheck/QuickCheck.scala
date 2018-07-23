package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a <- arbitrary[A]
      b <- oneOf(const(empty), genHeap)
    } yield insert(a,b)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  property("min2") = forAll{(a:Int, b: Int) =>
    val x = insert(b,insert(a,empty))
    if(a < b) findMin(x)== a
    else if (a > b) findMin(x) == b
    else findMin(x) == a | findMin(x) == a
  }
  property("empty1") = forAll{ a : Int =>
    val x = insert(a, empty)
    deleteMin(x)
    isEmpty(x)
  }

  property("sort1") = forAll{ h: H =>
    def sortTest( heap : H, xs: List[Int]): List[Int] = {
      if (isEmpty(heap)) xs
      else  sortTest(deleteMin(heap), findMin(heap) ::xs)
    }
    val testList = sortTest(h, Nil)
    testList == testList.sorted
  }

property("minMeld") = forAll{(x :H, y: H) =>
  val minMled = findMin(meld(x,y))
  val minX = findMin(x)
  val minY = findMin(y)
  val minO2 = if (minX <= minY) minX else minY
  minMled == minO2
}


  lazy val genMap: Gen[Map[Int,Int]] = oneOf(
    const(Map.empty[Int,Int]),
    for {
      k <- arbitrary[Int]
      v <- arbitrary[Int]
      m <- oneOf(const(Map.empty[Int,Int]), genMap)
    } yield m.updated(k, v)
  )

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

}
