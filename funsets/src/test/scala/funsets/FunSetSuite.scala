package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only elements that belong to both sets") {
    new TestSets {
      val u = union(s1, s2)
      val i = intersect(u, s2)
      assert(!contains(i, 1), "Intersect 1")
      assert(contains(i, 2), "Intersect 2")
      assert(!contains(i, 3), "Intersect 3")
    }
  }

  test("diff contains only the elements found in one set but not in the other") {
    new TestSets {
      val u = union(s1, union(s2, s3))
      assert(!contains(diff(u, s1), 1))
      assert(contains(diff(u, s1), 2))
      assert(contains(diff(u, s1), 3))

      assert(contains(diff(u, s2), 1))
      assert(!contains(diff(u, s2), 2))
      assert(contains(diff(u, s2), 3))

      assert(contains(diff(u, s3), 1))
      assert(contains(diff(u, s3), 2))
      assert(!contains(diff(u, s3), 3))
    }
  }

  test("filter contains only the elements from the given set that match the given predicate") {
    new TestSets {
      val u = union(s1, union(s2, s3))
      assert(contains(filter(u, (elem: Int) => elem == 1), 1))
      assert(!contains(filter(u, (elem: Int) => elem == 1), 2))
      assert(!contains(filter(u, (elem: Int) => elem == 1), 3))

      assert(!contains(filter(u, (elem: Int) => elem == 2), 1))
      assert(contains(filter(u, (elem: Int) => elem == 2), 2))
      assert(!contains(filter(u, (elem: Int) => elem == 2), 3))

      assert(!contains(filter(u, (elem: Int) => elem == 3), 1))
      assert(!contains(filter(u, (elem: Int) => elem == 3), 2))
      assert(contains(filter(u, (elem: Int) => elem == 3), 3))

      assert(!contains(filter(u, (elem: Int) => elem == 4), 4))
    }
  }

  test("forall returns whether all bounded integers within the given set satisfy the given predicate") {
    new TestSets {
      val u = union(s1, union(s2, s3))
      assert(forall(u, (elem: Int) => elem > 0))
      assert(!forall(u, (elem: Int) => elem < 0))

      val n = union(singletonSet(-1), union(singletonSet(-2), singletonSet(-3)))
      assert(forall(n, (elem: Int) => elem < 0))
    }
  }

  test("exists returns whether there exists a bounded integer within the given set that satisfies the given predicate") {
    new TestSets {
      val u = union(s1, s3)
      assert(exists(u, (elem: Int) => elem == 1))
      assert(!exists(u, (elem: Int) => elem == 2))
      assert(exists(u, (elem: Int) => elem == 3))
      assert(!exists(u, (elem: Int) => elem == 4))
    }
  }

  test("map returns a set transformed by applying a given function to each element of the given set") {
    new TestSets {
      val m = map(union(s1, union(s2, s3)), (elem: Int) => elem + 1)
      assert(!contains(m, 1))
      assert(contains(m, 2))
      assert(contains(m, 3))
      assert(contains(m, 4))
      assert(!contains(m, 5))
    }
  }
}
