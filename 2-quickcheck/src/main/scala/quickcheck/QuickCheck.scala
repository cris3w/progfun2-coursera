package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("min of heap") = forAll { (a: Int, b: Int) =>
    findMin(insert(a, insert(b, empty))) == min(a, b)
  }

  property("empty heap") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("order of mins 1") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        isEmpty(deleteMin(h)) || (findMin(h) <= findMin(deleteMin(h)) && isSorted(deleteMin(h)))
      }

    isSorted(h)
  }

  property("order of mins 2") = forAll { (h: H, i: H) =>
    def heapEqual(h: H, i: H): Boolean =
      if (isEmpty(h) && isEmpty(i)) true
      else {
        findMin(h) == findMin(i) && heapEqual(deleteMin(h), deleteMin(i))
      }

    heapEqual(meld(h, i), meld(deleteMin(h), insert(findMin(h), i)))
  }

  property("min of melding heaps") = forAll { (h: H, i: H) =>
    findMin(meld(h, i)) == min(findMin(h), findMin(i))
  }

}
