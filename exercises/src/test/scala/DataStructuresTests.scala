import org.scalatest._
import fpinscala.datastructures.List

class DataStructuresTests  extends FlatSpec with Matchers {

  "List foldRight" should "sum multiply and calculate length" in {

    val ns = List(1, 2, 3, 4)
    val ls = List(1.0, 2.0, 3.0, 4.0)

    List.sum2(ns) should be (10)
    List.product2(ls) should be (24)
    List.length(ls) should be (4)
  }

  "List foldLeft" should "sum multiply and calculate length" in {

    val ns = List(1, 2, 3, 4)
    val ls = List(1.0, 2.0, 3.0, 4.0)

    List.sum3(ns) should be (10)
    List.product3(ls) should be (24)
    List.length3(ls) should be (4)
  }

}