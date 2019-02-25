import org.scalatest._
import fpinscala.datastructures.List

class DataStructuresTests  extends FlatSpec with Matchers {

  val ss = List(1, 2)
  val ns = List(1, 2, 3, 4)
  val ls = List(1.0, 2.0, 3.0, 4.0)

  "List foldRight" should "sum multiply and calculate length" in {
    List.sum2(ns) should be(10)
    List.product2(ls) should be(24)
    List.length(ls) should be(4)
  }

  "List foldLeft" should "sum multiply and calculate length" in {
    List.sum3(ns) should be(10)
    List.product3(ls) should be(24)
    List.length3(ls) should be(4)
  }

  "Reverse" should "revert a list" in {
    List.reverse(ns) shouldBe List(4, 3, 2, 1)
  }

  "Append" should "append two lists using foldRight" in {
    List.appendViaFoldRight(ns, ss) shouldBe List(1, 2, 3, 4, 1, 2)
  }

  "Concat" should "concat three lists" in {
    List.concat(List(ss, ns, ss)) shouldBe List(1, 2, 1, 2, 3, 4, 1, 2)
  }

  "Add1" should "add 1 to each member of the list" in {
    List.add1(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  "DoubleToString" should "convert each member of the list to string" in {
    List.doubleToString(List(1.0, 2.0, 3.0)) shouldBe List("1.0", "2.0", "3.0")
  }

  "filter" should "filter out based on function" in {
    def f(a: Int): Boolean = a % 2 == 0

    List.filter(List(1, 2, 3))(f) shouldBe List(2)
  }

  "flatMap" should "flatten map" in {
    List.flatMap(ss)(i => List(i, i)) shouldBe List(1, 1, 2, 2)
  }

  "filterViaFlatMap" should "filter out based on function" in {
    def f(a: Int): Boolean = a % 2 == 0

    List.filterViaFlatMap(List(1, 2, 3))(f) shouldBe List(2)
  }

  "addPairWise" should "add two lists element by element" in {
    List.addPairwise(ns, ns) shouldBe List(2, 4, 6, 8)
    List.addPairwise(ns, ss) shouldBe List(2, 4)
  }

  "zipWith" should "zip two list together with function" in {
    List.zipWith(ns, ns)(_ + _) shouldBe List.addPairwise(ns, ns)
    List.zipWith(ns, ns)(_ * _) shouldBe List(1, 4, 9, 16)
  }
}