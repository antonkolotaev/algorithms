package sorting

import org.scalatest.{FlatSpec, Matchers}

class MergeSortedListsSpec extends FlatSpec with Matchers {

    import MergeSort.mergeSortedLists

    "empty lists" should "merge with an empty list" in {

        mergeSortedLists(List.empty[Int], List.empty[Int]) should be (List.empty[Int])

    }

    it should "merge with a non-empty list" in {

        mergeSortedLists(List.empty[Int], List(1,3,4)) should be (List(1,3,4))

    }

    "non-empty lists" should "merge with empty lists" in {

        mergeSortedLists(List(1,3,8), List.empty[Int]) should be (List(1,3,8))

    }

    it should "merge with non-empty lists" in  {

        mergeSortedLists(List(1,2,3,8), List(4,5,6,7,9,10)) should be (1 to 10)

    }

}
