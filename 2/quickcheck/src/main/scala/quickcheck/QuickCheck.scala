package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == math.min(a, b)
  }

  property("deleteMin") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("sorted") = forAll { (h: H) =>
    asList(h) == asList(h).sorted
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)

    val h3 = meld(h1, h2)
    val m3 = findMin(h3)

    m3 == math.min(m1, m2)
  }

  property("meld2") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)

    val meld1 = meld(h1, h2)
    val meld2 = meld(deleteMin(h1), insert(m1, h2))
    val meld3 = meld(insert(m2, h1), deleteMin(h2))

    asList(meld1) == asList(meld2) && asList(meld2) == asList(meld3)
  }




  def asList(h: H): List[Int] = {
    if(isEmpty(h)) Nil else findMin(h) :: asList(deleteMin(h))
  }
}
